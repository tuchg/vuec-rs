use std::{cell::RefCell, ops::Deref, rc::Rc};

use sourcemap::{SourceMap, SourceMapBuilder};

use crate::{
    ast::{
        codegen::{CodegenNode, SSRCodegenNode},
        el::TemplateLit,
        expr,
        expr::{
            ArrayExprNode, AssignmentExpr, CacheExpr, CallExpr, CallExprArgsNode, CalleeNode,
            CompoundChildNode, ExprNode, FuncArgsNode, FuncBodyNode, FuncExpr, FuncReturns,
            ObjExpr, SequenceExpr, SimpleExpr,
        },
        js_child::{
            JSChildNode,
            JSChildNode::{ArrayExpr, Expr},
            VNodeCall,
        },
        parent::Root,
        stmt::{IfStmt, IfStmtAltNode, ReturnStmt, ReturnsNode},
        template_child::{Comment, Interpolation, TemplateChildNode, Text},
        utils::{Position, SourceLocation, LOC_STUB},
        Node, NodeType,
    },
    options::{CodegenMode, CodegenOptions},
    runtime_helpers::{RuntimeHelper, RuntimeHelper::*},
    transform::ImportItem,
    utils::{
        get_vnode_block_helper, get_vnode_helper, is_simple_identifier, json_stringify,
        to_valid_asset_id,
    },
    __BROWSER__, __COMPAT__, __DEV__,
};

const PURE_ANNOTATION: &str = "/*#__PURE__*/";

pub fn generate(
    mut ast: Node<Root>,
    options: Option<CodegenOptions>,
    on_context_created: Option<fn(context: &CodegenContext)>,
) -> CodegenResult {
    let options = options.unwrap_or_default();
    let is_setup_inlined = !__BROWSER__ && options.inner.inline;
    let mut ctx = CodegenContext::new(&ast, options.clone());

    // preambles
    // in setup() inline mode, the preamble is generated in a sub context
    // and returned separately.
    let mut preamble_ctx = if is_setup_inlined {
        CodegenContext::new(&ast, options)
    } else {
        ctx.clone()
    };

    let gen_scope_id = !__BROWSER__
        && ctx.options.scope_id.is_some()
        && matches!(ctx.options.mode, CodegenMode::Module);

    if let Some(on_context_created) = on_context_created {
        on_context_created(&ctx);
    }

    if !__BROWSER__ && matches!(ctx.options.mode, CodegenMode::Module) {
        preamble_ctx.gen_module_preamble(&mut ast, gen_scope_id, is_setup_inlined);
    } else {
        preamble_ctx.gen_function_preamble(&mut ast);
    }

    let has_helpers = !ast.inner.helpers.is_empty();
    let use_with_block =
        !ctx.options.inner.prefix_identifiers && !matches!(ctx.options.mode, CodegenMode::Module);

    // enter render function
    let func_name = if ctx.options.inner.ssr {
        "ssrRender"
    } else {
        "render"
    };
    let mut args = if ctx.options.inner.ssr {
        vec!["_ctx", "_push", "_parent", "_attrs"]
    } else {
        vec!["_ctx", "_cache"]
    };
    if !__BROWSER__ && ctx.options.inner.binding_metadata.is_some() && !ctx.options.inner.inline {
        args.extend(&["$props", "$setup", "$data", "$options"]);
    }

    let signature = if !__BROWSER__ && ctx.options.inner.is_ts {
        format!("export function {func_name}(")
    } else {
        format!("function {func_name}(")
    };
    if is_setup_inlined {
        ctx.push(&format!("({signature}) => {{"), None);
    } else {
        ctx.push(&format!("function {func_name}({signature}) {{"), None);
    }
    ctx.indent();

    if use_with_block {
        ctx.push(r"with (_ctx) {", None);
        ctx.indent();
        // function mode const declarations should be inside with block
        // also they should be renamed to avoid collision with user properties
        if has_helpers {
            let helpers = ast
                .inner
                .helpers
                .iter()
                .map(alias_helper)
                .collect::<Vec<_>>()
                .join(", ");
            ctx.push(&format!("const {{ {helpers} }} = _Vue"), None);
            ctx.push("\n", None);
            ctx.newline(None)
        }
    }
    // generate asset resolution statements
    if !ast.inner.components.is_empty() {
        ctx.gen_assets(&ast.inner.components, Assets::Component);
        if !ast.inner.directives.is_empty() && ast.inner.temps > 0 {
            ctx.newline(None);
        }
    }
    if !ast.inner.directives.is_empty() {
        ctx.gen_assets(&ast.inner.directives, Assets::Directive);
        if ast.inner.temps > 0 {
            ctx.newline(None);
        }
    }
    if __COMPAT__ && let Some(filters) = &ast.inner.filters && !filters.is_empty() {
        ctx.newline(None);
        ctx.gen_assets(filters, Assets::Filter);
        ctx.newline(None);
    }
    if ast.inner.temps > 0 {
        ctx.push("let ", None);
        for i in 0..ast.inner.temps {
            let string = if i > 0 {
                format!(", _temp{i}")
            } else {
                format!("_temp{i}")
            };
            ctx.push(&string, None);
        }
    }

    if !ast.inner.components.is_empty() || !ast.inner.directives.is_empty() || ast.inner.temps > 0 {
        ctx.push("\n", None);
        ctx.newline(None);
    }

    // generate the VNode tree expression
    if ctx.options.inner.ssr {
        ctx.push("return ", None);
    }
    if let Some(codegen_node) = &ast.inner.codegen_node {
        ctx.push("(", None);
        ctx.gen_node(codegen_node);
        ctx.push(")", None);
    } else {
        ctx.push("null", None);
    }

    if use_with_block {
        ctx.deindent(false);
        ctx.push("}", None);
    }

    ctx.deindent(false);
    ctx.push("}", None);

    CodegenResult {
        code: ctx.code,
        preamble: if is_setup_inlined {
            preamble_ctx.code.to_string()
        } else {
            String::new()
        },
        ast,
        map: ctx.map.map(SourceMapBuilder::into_sourcemap),
    }
}

pub struct CodegenResult {
    pub code: String,
    pub preamble: String,
    pub ast: Node<Root>,
    pub map: Option<SourceMap>,
}

pub struct CodegenContext {
    source: String,
    code: String,
    pos: Position,
    indent_lv: usize,
    pure: bool,
    map: Option<SourceMapBuilder>,
    options: CodegenOptions,
}

impl Clone for CodegenContext {
    fn clone(&self) -> Self {
        let map = if let Some(map) = &self.map {
            Some(SourceMapBuilder::new(map.get_file()))
        } else {
            None
        };
        Self {
            source: self.source.clone(),
            code: self.code.clone(),
            pos: self.pos.clone(),
            indent_lv: self.indent_lv,
            pure: self.pure,
            map,
            options: self.options.clone(),
        }
    }
}

#[derive(Clone)]
pub enum Assets {
    Component,
    Directive,
    Filter,
}

impl From<Assets> for &str {
    fn from(asset: Assets) -> Self {
        match asset {
            Assets::Component => "component",
            Assets::Directive => "directive",
            Assets::Filter => "filter",
        }
    }
}

impl CodegenContext {
    pub fn new(ast: &Node<Root>, mut options: CodegenOptions) -> Self {
        options.inner.prefix_identifiers = matches!(options.mode, CodegenMode::Module);

        let mut context = Self {
            source: ast.inner.source.to_string(),
            code: "".to_string(),
            pos: Default::default(),
            indent_lv: 0,
            pure: false,
            map: None,
            options,
        };
        if !__BROWSER__ && context.options.source_map {
            // lazy require source-map implementation, only in non-browser builds
            let mut builder = SourceMapBuilder::new(None);
            builder.add_source(&ast.inner.source);
            context.map = Some(builder);
        }
        context
    }

    fn gen_module_preamble(&mut self, ast: &mut Node<Root>, gen_scope_id: bool, inline: bool) {
        if gen_scope_id && !ast.inner.hoists.is_empty() {
            ast.inner.helpers.insert(PushScopeId);
            ast.inner.helpers.insert(PopScopeId);
        }
        // generate import statements for helpers
        if !ast.inner.helpers.is_empty() {
            if self.options.optimize_imports {
                // when bundled with webpack with code-split, calling an import binding
                // as a function leads to it being wrapped with `Object(a.b)` or `(0,a.b)`,
                // incurring both payload size increase and potential perf overhead.
                // therefore we assign the imports to variables (which is a constant ~50b
                // cost per-component instead of scaling with template size)

                self.push(
                    &format!(
                        "import {{ {} }} from {}\n",
                        ast.inner
                            .helpers
                            .iter()
                            .map(|h| h.into_str())
                            .collect::<Vec<_>>()
                            .join(", "),
                        json_stringify(&self.options.runtime_module_name)
                    ),
                    None,
                );
                self.push(
                    &format!(
                        "\n// Binding optimization for webpack code-split\nconst {}\n",
                        ast.inner
                            .helpers
                            .iter()
                            .map(|h| format!("_{s} = {s}", s = h.into_str()))
                            .collect::<Vec<_>>()
                            .join(", ")
                    ),
                    None,
                );
            } else {
                self.push(
                    &format!(
                        "import {{ {} }} from {}\n",
                        ast.inner
                            .helpers
                            .iter()
                            .map(|h| format!("{s} as _{s}", s = h.into_str()))
                            .collect::<Vec<_>>()
                            .join(", "),
                        json_stringify(&self.options.runtime_module_name)
                    ),
                    None,
                );
            }
        }

        if let Some(ssr_helpers) = &ast.inner.ssr_helpers {
            self.push(
                &format!(
                    "import {{ {} }} from {}\n",
                    ssr_helpers
                        .iter()
                        .map(|h| format!("{s} as _{s}", s = h.into_str()))
                        .collect::<Vec<_>>()
                        .join(", "),
                    self.options.ssr_runtime_module_name
                ),
                None,
            );
        }

        if !ast.inner.imports.is_empty() {
            self.gen_imports(&ast.inner.imports);
            self.newline(None);
        }
        self.gen_hoists(&mut ast.inner.hoists);
        self.newline(None);
        if !inline {
            self.push("export ", None);
        }
    }

    fn gen_imports(&mut self, imports: &Vec<ImportItem>) {
        if imports.is_empty() {
            return;
        }
        for import in imports {
            self.push("import ", None);
            let node = &CodegenNode::JSChild(Expr(Box::from(import.expr.clone())));
            self.gen_node(node);
            self.push(&format!(" from '{}'", import.path), None);
            self.newline(None);
        }
    }

    fn gen_function_preamble(&mut self, ast: &mut Node<Root>) {
        let vue_binding = if !__BROWSER__ && self.options.inner.ssr {
            format!(
                "require({})",
                json_stringify(&self.options.runtime_module_name)
            )
        } else {
            self.options.runtime_global_name.to_string()
        };

        // Generate const declaration for helpers
        // In prefix mode, we place the const declaration at top so it's done
        // only once; But if we not prefixing, we place the declaration inside the
        // with block so it doesn't incur the `in` check cost for every helper access.
        if !ast.inner.helpers.is_empty() {
            if self.options.inner.prefix_identifiers {
                let helpers = ast
                    .inner
                    .helpers
                    .iter()
                    .map(alias_helper)
                    .collect::<Vec<_>>()
                    .join(", ");
                self.push(&format!("const {{ {helpers} }} = {vue_binding}\n"), None);
            } else {
                // "with" mode.
                // save Vue in a separate variable to avoid collision
                self.push(&format!("const _Vue = {vue_binding}\n"), None);
                // in "with" mode, helpers are declared inside the with block to avoid
                // has check cost, but hoists are lifted out of the function - we need
                // to provide the helper here.
                if !ast.inner.hoists.is_empty() {
                    let static_helpers = [
                        CreateVNode,
                        CreateElementVNode,
                        CreateComment,
                        CreateText,
                        CreateStatic,
                    ]
                    .into_iter()
                    .filter(|h| ast.inner.helpers.contains(h))
                    .map(|h| alias_helper(&h))
                    .collect::<Vec<_>>()
                    .join(", ");
                    self.push(&format!("const {{ {static_helpers} }} = _Vue\n"), None);
                }
            }
            self.push("\n", None);
            self.newline(None);
        }
        // generate variables for ssr helpers
        if !__BROWSER__ && let Some(ssr_helpers) = &ast.inner.ssr_helpers && !ssr_helpers.is_empty() {
            // ssr guarantees prefixIdentifier: true
            let ssr_helpers = ssr_helpers
                .iter()
                .map(alias_helper)
                .collect::<Vec<_>>()
                .join(", ");
            self.push(&format!("const {{ {} }} = require(\"{}\")\n", ssr_helpers, self.options.ssr_runtime_module_name), None);
        }
        self.gen_hoists(&mut ast.inner.hoists);
        self.newline(None);
        self.push("return ", None);
    }

    fn gen_hoists(&mut self, hoists: &mut Vec<JSChildNode>) {
        if hoists.is_empty() {
            return;
        }
        self.pure = true;
        let gen_scope_id = !__BROWSER__
            && self.options.scope_id.is_some()
            && matches!(self.options.mode, CodegenMode::Function);
        self.newline(None);
        // generate inlined withScopeId helper
        if gen_scope_id {
            // todo scope_id
            let scope_id = self.options.scope_id.as_ref().unwrap();
            self.push(
                &format!(
                    "const _withScopeId = n => ({}(\"{}\"),n=n(),{}(),n)",
                    Self::helper(PopScopeId),
                    scope_id,
                    Self::helper(PopScopeId)
                ),
                None,
            );
            self.newline(None);
        }

        hoists.iter().enumerate().for_each(|(i, hoist)| {
            let need_scope_id_wrapper =
                gen_scope_id && matches!(&hoists[i], JSChildNode::VNodeCall(_));
            self.push(
                &format!(
                    "const _hoisted_{} = {}",
                    i + 1,
                    if need_scope_id_wrapper {
                        format!("{PURE_ANNOTATION} _withScopeId(() => ")
                    } else {
                        String::with_capacity(0)
                    }
                ),
                None,
            );
            let node = CodegenNode::JSChild(hoists[i].clone());
            self.gen_node(&node);

            if need_scope_id_wrapper {
                self.push(")", None);
            }
            self.newline(None);
        });
        self.pure = false;
    }

    fn gen_assets(&mut self, assets: &[String], kind: Assets) {
        let resolver = Self::helper(match kind {
            Assets::Filter if __COMPAT__ => ResolveFilter,
            Assets::Component => ResolveComponent,
            _ => ResolveDirective,
        });

        assets.iter().enumerate().for_each(|(i, asset)| {
            let mut id = asset.as_str();
            // potential component implicit self-reference inferred from SFC filename
            let maybe_self_ref = id.ends_with("__self");
            if maybe_self_ref {
                id = &id[..id.len() - 6];
            }
            self.push(
                &format!(
                    "const {} = {}({}{}){}",
                    to_valid_asset_id(id, kind.clone()),
                    resolver,
                    id,
                    if maybe_self_ref { ", true" } else { "" },
                    if self.options.inner.is_ts { "!" } else { "" }
                ),
                None,
            );

            if i < assets.len() - 1 {
                self.newline(None);
            }
        });
    }

    fn gen_node(&mut self, node: &CodegenNode) {
        match node {
            CodegenNode::Str(s) => {
                self.push(s, None);
            }
            CodegenNode::RH(rh) => {
                self.push(&Self::helper(*rh), None);
            }
            CodegenNode::TemplateChild(template) => match template {
                TemplateChildNode::Element(n) => {
                    self.gen_node(&n.codegen().unwrap());
                }
                TemplateChildNode::If(n) => {
                    // self.gen_node(&n.inner.codegen.into()),
                }
                TemplateChildNode::For => {}

                TemplateChildNode::Text(n) => self.gen_text(n.clone()),
                TemplateChildNode::Interpolation(n) => self.gen_interpolation(n.clone()),
                TemplateChildNode::CompoundExpr(n) => self.gen_compound_expr(node),
                TemplateChildNode::Comment(n) => self.gen_comment(n.clone()),
                TemplateChildNode::TextCall => {}
                TemplateChildNode::IfBranch(n) => {
                    // noop
                }
            },
            CodegenNode::JSChild(js) => match js {
                JSChildNode::VNodeCall(n) => {
                    self.gen_vnode_call(n.deref().clone());
                }
                JSChildNode::CallExpr(n) => self.gen_call_expr(n.deref().clone()),
                JSChildNode::ObjExpr(n) => self.gen_object_expr(n.deref().clone()),
                JSChildNode::ArrayExpr(n) => {}
                JSChildNode::Expr(n) => match n.deref() {
                    ExprNode::Simple(n) => self.gen_expr(n),
                    ExprNode::Compound(n) => self.gen_compound_expr(node),
                },
                JSChildNode::FuncExpr(n) => {
                    self.gen_function_expr(n.deref().clone());
                }
                JSChildNode::CondExpr(n) => {
                    self.gen_cond_expr(n.deref().clone());
                }
                JSChildNode::CacheExpr(n) => self.gen_cache_expr(n.deref().clone()),
                JSChildNode::AssignmentExpr(n) if !__BROWSER__ => {
                    self.gen_assignment_expr(n.deref().clone())
                }
                JSChildNode::SequenceExpr(n) if !__BROWSER__ => {
                    self.gen_sequence_expr(n.deref().clone())
                }
                JSChildNode::Str(s) => {
                    self.push(s, None);
                }
                _ => {}
            },
            CodegenNode::SSR(ssr) if !__BROWSER__ => match ssr {
                SSRCodegenNode::BlockStmt(n) => {
                    // self.gen_node_list(n.inner.body.iter().map(|f|).collect(), Some(true), None),
                }
                SSRCodegenNode::TemplateLiteral(n) => self.gen_template_literal(n.clone()),
                SSRCodegenNode::IfStmt(n) => self.gen_if_stmt(n.clone()),
                SSRCodegenNode::AssignmentExpr(n) => self.gen_assignment_expr(n.deref().clone()),
                SSRCodegenNode::ReturnStmt(n) => self.gen_return_stmt(n.deref().clone()),
                SSRCodegenNode::SeqExpr(n) => self.gen_sequence_expr(n.deref().clone()),
            },
            _ => {}
        }
        // match node.kind() {
        //     NodeType::Element | NodeType::If | NodeType::For => {
        //         // __DEV__
        //         //     && assert!(
        //         //         node.codegen.is_some(),
        //         //         "Codegen node is missing for element/if/for node. Apply appropriate \
        //         //          transforms first."
        //         //     );
        //         let mut codegen_node = None;
        //         match node {
        //             RootCodegenNode::TemplateChild(TemplateChildNode::Element(el)) => match el {
        //                 ElementNode::Plain(n) => codegen_node = n.inner.codegen_node,
        //                 ElementNode::Component(n) => {}
        //                 ElementNode::SlotOutlet(n) => {}
        //                 _ => {}
        //             },
        //             RootCodegenNode::TemplateChild(TemplateChildNode::If(n)) => {
        //                 n.inner.codegen_node
        //             }
        //             RootCodegenNode::TemplateChild(TemplateChildNode::For) => {}
        //             _ => {}
        //         }
        //         self.gen_node(node.codegen_node)
        //     }
        //     NodeType::Text => self.gen_text(node),
        //     SimpleExpr => self.gen_expr(node),
        //     Interpolation => self.gen_interpolation(node),
        //     TextCall => self.gen_node(node.codegen_node),
        //     CompoundExpr => self.gen_compound_expr(node),
        //     Comment => self.gen_comment(node),
        //     VNodeCall => self.gen_vnode_call(node),
        //
        //     JSCallExpr => self.gen_call_expr(node),
        //     JSObjectExpr => self.gen_object_expr(node),
        //     JSArrayExpr => self.gen_array_expr(node),
        //     JSFunctionExpr => self.gen_function_expr(node),
        //     JSConditionalExpr => self.gen_conditional_expr(node),
        //     JSCacheExpr => self.gen_cache_expr(node),
        //
        //     // SSR only types
        //     JSTemplateLiteral if !__BROWSER__ => self.gen_template_literal(node),
        //     JSIfStmt if !__BROWSER__ => self.gen_if_stmt(node),
        //     JSAssignmentExpr if !__BROWSER__ => self.gen_assignment_expr(node),
        //     JSSequenceExpr if !__BROWSER__ => self.gen_sequence_expr(node),
        //     JSReturnStmt if !__BROWSER__ => self.gen_return_stmt(node),
        //     // istanbul ignore next
        //     IfBranch => { /*noop*/ }
        //     k => {
        //         if __DEV__ {
        //             // make sure we exhaust all possible types
        //             assert!(false, "unhandled codegen node type: {:#?}", k)
        //         }
        //     }
        // }
    }

    fn helper(key: RuntimeHelper) -> String {
        format!(" _${}", key.into_str())
    }

    fn push(&mut self, code: &str, node: Option<PushedNode>) {
        self.code.push_str(code);
        if !__BROWSER__ && self.map.is_some() {
            if let Some(node) = &node {
                let mut name: Option<&str> = None;
                if let NodeType::SimpleExpr = node.kind {
                    let ori_content = node.content;
                    let content = ori_content.trim_start_matches("_ctx.");
                    if content != ori_content && is_simple_identifier(content) {
                        name = Some(content);
                    }
                    self.add_mapping(node.loc.start, name);
                }
            }

            self.pos
                .advance_position_with_mutation(code, self.source.len());

            if let Some(node) = node {
                if node.loc != &LOC_STUB {
                    self.add_mapping(node.loc.end, None);
                }
            }
        }
    }

    fn indent(&mut self) {
        self.indent_lv += 1;
        self.newline(Some(self.indent_lv))
    }

    fn deindent(&mut self, without_newline: bool) {
        if without_newline {
            self.indent_lv -= 1;
        } else {
            self.indent_lv -= 1;
            self.newline(Some(self.indent_lv));
        }
    }

    fn newline(&mut self, n: Option<usize>) {
        let n = n.unwrap_or(self.indent_lv);
        let mut line = "  ".repeat(n);
        line.insert(0, '\n');
        self.push(&line, None);
    }

    fn add_mapping(&mut self, loc: Position, name: Option<&str>) {
        if let Some(map) = &mut self.map {
            map.add(
                self.pos.line as u32,
                (self.pos.column - 1) as u32,
                loc.line as u32,
                (loc.column - 1) as u32,
                Some(&self.options.inner.filename),
                name,
            );
        }
    }

    fn gen_text(&mut self, node: Node<Text>) {
        self.push(
            &json_stringify(&node.inner.content),
            Some(PushedNode {
                kind: NodeType::Text,
                is_static: false,
                content: &node.inner.content,
                loc: &node.loc,
            }),
        );
    }

    fn gen_expr(&mut self, node: &Node<SimpleExpr>) {
        self.push(
            &if node.inner.is_static {
                json_stringify(&node.inner.content)
            } else {
                node.inner.content.clone()
            },
            Some(PushedNode {
                kind: NodeType::SimpleExpr,
                is_static: false,
                content: &node.inner.content,
                loc: &node.loc,
            }),
        );
    }

    fn gen_interpolation(&mut self, node: Node<Interpolation>) {
        if self.pure {
            self.push(PURE_ANNOTATION, None);
        }
        self.push(&format!("{}(", Self::helper(ToDisplayString)), None);
        self.gen_node(&CodegenNode::JSChild(Expr(Box::from(node.inner.content))));
        self.push(")", None);
    }

    fn gen_compound_expr(&mut self, node: &CodegenNode) {
        if let CodegenNode::JSChild(Expr(expr)) = node {
            if let ExprNode::Compound(compound_expr) = expr.as_ref() {
                for child in &compound_expr.inner.children {
                    match &child.inner {
                        CompoundChildNode::Str(s) => {
                            self.push(s, None);
                        }
                        _ => {
                            self.gen_node(node);
                        }
                    }
                }
            }
        }
    }

    fn gen_comment(&mut self, node: Node<Comment>) {
        if self.pure {
            self.push(PURE_ANNOTATION, None);
        }
        self.push(
            &format!(
                "{}({})",
                Self::helper(CreateComment),
                json_stringify(&node.inner.content)
            ),
            Some(PushedNode {
                kind: NodeType::Comment,
                is_static: false,
                loc: &node.loc,
                content: &node.inner.content,
            }),
        )
    }

    fn gen_vnode_call(&mut self, node: Node<VNodeCall>) {
        if node.inner.directives.is_some() {
            self.push(&format!("{}(", Self::helper(WithDirectives)), None);
        }
        if node.inner.is_block {
            self.push(
                &format!(
                    "{}({}), ",
                    Self::helper(OpenBlock),
                    if node.inner.disable_tracking {
                        "true"
                    } else {
                        ""
                    }
                ),
                None,
            );
        }
        if self.pure {
            self.push(PURE_ANNOTATION, None);
        }
        let call_helper = if node.inner.is_block {
            get_vnode_block_helper(self.options.inner.in_ssr, node.inner.is_component)
        } else {
            get_vnode_helper(self.options.inner.in_ssr, node.inner.is_component)
        };
        self.push(
            &format!("{}(", call_helper.into_str()),
            Some(PushedNode {
                kind: node.kind,
                is_static: false,
                content: "",
                loc: &node.loc,
            }),
        );
        // self.gen_node_list()
        self.push(")", None);
        if node.inner.is_block {
            self.push(")", None);
        }

        if let Some(dirs) = node.inner.directives {
            self.push(", ", None);
            self.gen_node(&CodegenNode::JSChild(ArrayExpr(Box::from(dirs))));
            self.push(")", None);
        }
    }

    fn gen_call_expr(&mut self, node: Node<CallExpr>) {
        let callee = match &node.inner.callee {
            CalleeNode::Str(s) => s.clone(),
            CalleeNode::RH(rh) => Self::helper(*rh),
        };
        if self.pure {
            self.push(
                PURE_ANNOTATION,
                Some(PushedNode {
                    kind: node.kind,
                    is_static: false,
                    content: "",
                    loc: &node.loc,
                }),
            );
        }
        self.push(&format!("{callee}("), None);
        let nodes = node
            .inner
            .args
            .iter()
            .map(|arg| match arg {
                CallExprArgsNode::Str(s) => CodegenNode::Str(s.clone()),
                CallExprArgsNode::RH(r) => CodegenNode::RH(*r),
                CallExprArgsNode::JSChild(n) => CodegenNode::JSChild(n.clone()),
                CallExprArgsNode::SSR(n) => CodegenNode::SSR(n.clone()),
                CallExprArgsNode::TemplateChild(n) => CodegenNode::TemplateChild(n.clone()),
                CallExprArgsNode::TemplateChilds(n) => CodegenNode::TemplateChilds(n.clone()),
            })
            .collect();
        self.gen_node_list(nodes, None, None);
        self.push(")", None);
    }

    fn gen_object_expr(&mut self, node: Node<ObjExpr>) {
        if !node.inner.properties.is_empty() {
            self.push(
                "{}",
                Some(PushedNode {
                    kind: node.kind,
                    is_static: false,
                    content: "",
                    loc: &node.loc,
                }),
            );
            return;
        }

        let multilines = node.inner.properties.len() > 1
            || ((!__BROWSER__ || __DEV__)
                && node.inner.properties.iter().any(|p| match &p.inner.value {
                    Expr(expr) => !matches!(expr.as_ref(), ExprNode::Simple(_)),
                    _ => true,
                }));
        self.push(if multilines { "{" } else { "{ " }, None);
        if multilines {
            self.indent();
        }
        node.inner.properties.iter().enumerate().for_each(|(i, p)| {
            // key

            self.push(": ", None);
            // value
            self.gen_node(&CodegenNode::JSChild(p.inner.value.clone()));
            if i < node.inner.properties.len() - 1 {
                // will only reach this if it's multilines
                self.push(",", None);
                self.newline(None);
            }
        });

        if multilines {
            self.deindent(false);
        }
        self.push(if multilines { "}" } else { " }" }, None);
    }

    fn gen_expr_as_property_key(&mut self, node: ExprNode) {
        match node {
            ExprNode::Simple(n) => {
                if n.inner.is_static {
                    // only quote keys if necessary
                    let text = if is_simple_identifier(n.inner.content.as_str()) {
                        n.inner.content
                    } else {
                        json_stringify(n.inner.content.as_str())
                    };
                    self.push(&text, None);
                } else {
                    self.push(&format!("[{}]", n.inner.content), None);
                }
            }
            ExprNode::Compound(n) => {
                self.push("[", None);
                self.gen_compound_expr(&CodegenNode::JSChild(Expr(Box::from(ExprNode::Compound(
                    n,
                )))));
                self.push("]", None);
            }
        };
    }

    fn gen_array_expr(&mut self, node: ArrayExprNode) {
        match node {
            ArrayExprNode::DirArgs(n) => {
                // self.gen_node_list_as_array(n);
            }
            ArrayExprNode::DynSlotEntries(n) => {
                // self.gen_node_list_as_array(n);
            }
        }
    }

    fn gen_function_expr(&mut self, node: Node<FuncExpr>) {
        if node.inner.is_slot {
            // wrap slot functions with owner context
            self.push(&format!("_{}(", WithCtx.into_str()), None);
        }
        self.push(
            "(",
            Some(PushedNode {
                kind: node.kind,
                is_static: false,
                content: "",
                loc: &node.loc,
            }),
        );
        self.gen_node_list(
            node.inner
                .params
                .iter()
                .map(|p| match p {
                    FuncArgsNode::Expr(n) => CodegenNode::JSChild(Expr(Box::from(n.clone()))),
                    FuncArgsNode::Str(n) => CodegenNode::Str(n.clone()),
                })
                .collect(),
            None,
            None,
        );
        self.push(") => ", None);
        let has_body = node.inner.body.is_some();
        if node.inner.newline || has_body {
            self.push("{", None);
            self.indent()
        }
        if let Some(returns) = node.inner.returns {
            if node.inner.newline {
                self.push("return ", None);
            }
            match returns {
                FuncReturns::TemplateChild(n) => {
                    let nodes = n
                        .iter()
                        .map(|n| CodegenNode::TemplateChild(n.clone()))
                        .collect();
                    self.gen_node_list(nodes, None, None);
                }
                FuncReturns::JSChild(n) => {
                    self.gen_node(&CodegenNode::JSChild(n));
                }
            }
        } else if let Some(body) = node.inner.body {
            let node = match body {
                FuncBodyNode::BlockStmt(n) => CodegenNode::SSR(SSRCodegenNode::BlockStmt(n)),
                FuncBodyNode::IfStmt(n) => CodegenNode::SSR(SSRCodegenNode::IfStmt(n)),
            };
            self.gen_node(&node);
        }
        if node.inner.newline || has_body {
            self.deindent(false);
            self.push("}", None);
        }
        if node.inner.is_slot {
            if __COMPAT__ && node.inner.is_non_scoped_slot {
                self.push(", undefined, true", None);
            }
            self.push(")", None);
        }
    }

    fn gen_cond_expr(&mut self, node: Node<expr::CondExpr>) {
        let is_test_simple_expr = match &node.inner.test {
            Expr(expr) => {
                if let ExprNode::Simple(expr) = &**expr {
                    let need_parens = !is_simple_identifier(&expr.inner.content);
                    if need_parens {
                        self.push("(", None);
                    }
                    self.gen_expr(expr);
                    if need_parens {
                        self.push(")", None);
                    }
                    true
                } else {
                    false
                }
            }
            _ => false,
        };
        if !is_test_simple_expr {
            self.push("(", None);
            self.gen_node(&CodegenNode::JSChild(node.inner.test));
            self.push(")", None);
        }

        if node.inner.newline {
            self.indent();
        }
        self.indent_lv += 1;
        if !node.inner.newline {
            self.push(" ", None);
        }
        self.push("? ", None);
        self.gen_node(&CodegenNode::JSChild(node.inner.consequent));
        self.indent_lv -= 1;
        if node.inner.newline {
            self.newline(None);
        }
        if !node.inner.newline {
            self.push(" ", None);
        }
        self.push(": ", None);
        let is_nested = matches!(node.inner.alternate, JSChildNode::CondExpr(_));
        if !is_nested {
            self.indent_lv += 1;
        }
        self.gen_node(&CodegenNode::JSChild(node.inner.alternate));
        if !is_nested {
            self.indent_lv -= 1;
        }
        if node.inner.newline {
            self.deindent(true /* without newline */);
        }
    }

    fn gen_cache_expr(&mut self, node: Node<CacheExpr>) {
        self.push(&format!("_cache[{}] || (", node.inner.index), None);
        if node.inner.is_vnode {
            self.indent();
            self.push(&format!("{}(-1),", Self::helper(SetBlockTracking)), None);
            self.newline(None);
        }
        self.push(&format!("_cache[{}] = ", node.inner.index), None);
        self.gen_node(&CodegenNode::JSChild(node.inner.value));
        if node.inner.is_vnode {
            self.push(",", None);
            self.newline(None);
            self.push(&format!("{}(1),", Self::helper(SetBlockTracking)), None);
            self.newline(None);
            self.push(&format!("_cache[{}]", node.inner.index), None);
            self.deindent(false);
        }
        self.push(")", None);
    }

    fn gen_template_literal(&mut self, node: Node<TemplateLit>) {
        self.push("`", None);
        let len = node.inner.elements.len();
        let multilines = len > 3;
        for element in node.inner.elements.into_iter() {
            if let JSChildNode::Str(s) = element {
                let chars_to_escape = ["`", "$", "\\"];
                let mut replaced = s;
                for c in chars_to_escape {
                    replaced = replaced.replace(c, &format!("\\{c}"));
                }
                self.push(&replaced, None);
            } else {
                self.push("${", None);
                if multilines {
                    self.indent();
                }
                self.gen_node(&CodegenNode::JSChild(element));
                if multilines {
                    self.deindent(false);
                }
                self.push("}", None);
            }
        }
        self.push("`", None);
    }

    fn gen_if_stmt(&mut self, node: Node<IfStmt>) {
        self.push("if (", None);
        self.gen_node(&CodegenNode::JSChild(Expr(Box::from(node.inner.test))));
        self.push(") {", None);
        self.indent();
        self.gen_node(&CodegenNode::SSR(SSRCodegenNode::BlockStmt(
            node.inner.consequent,
        )));
        self.deindent(false);
        self.push("}", None);
        if let Some(alternate) = node.inner.alternate {
            self.push(" else ", None);
            if let IfStmtAltNode::IfStmt(if_stmt) = *alternate {
                self.gen_node(&CodegenNode::SSR(SSRCodegenNode::IfStmt(if_stmt)));
            } else {
                self.push("{", None);
                self.indent();
                let node = match *alternate {
                    IfStmtAltNode::IfStmt(stmt) => SSRCodegenNode::IfStmt(stmt),
                    IfStmtAltNode::BlockStmt(stmt) => SSRCodegenNode::BlockStmt(stmt),
                    IfStmtAltNode::ReturnStmt(stmt) => SSRCodegenNode::ReturnStmt(stmt),
                };
                self.gen_node(&CodegenNode::SSR(node));
                self.deindent(false);
                self.push("}", None);
            }
        }
    }
    fn gen_assignment_expr(&mut self, node: Node<AssignmentExpr>) {
        self.gen_node(&CodegenNode::JSChild(Expr(Box::from(ExprNode::Simple(
            node.inner.left,
        )))));
        self.push(" = ", None);
        self.gen_node(&CodegenNode::JSChild(node.inner.right));
    }
    fn gen_sequence_expr(&mut self, node: Node<SequenceExpr>) {
        self.push("(", None);
        let expr = node
            .inner
            .exprs
            .iter()
            .map(|n| CodegenNode::JSChild(n.clone()))
            .collect();
        self.gen_node_list(expr, None, None);
        self.push(")", None)
    }
    fn gen_return_stmt(&mut self, node: Node<ReturnStmt>) {
        self.push("return ", None);
        match node.inner.returns {
            ReturnsNode::TemplateChild(returns) => {
                let node = returns
                    .iter()
                    .map(|n| CodegenNode::TemplateChild(n.clone()))
                    .collect();
                self.gen_node_list(node, None, None);
            }
            ReturnsNode::JSChild(returns) => {
                self.gen_node(&CodegenNode::JSChild(returns));
            }
        }
    }

    fn gen_node_list(
        &mut self,
        nodes: Vec<CodegenNode>,
        multilines: Option<bool>,
        comma: Option<bool>,
    ) {
        let multilines = multilines.unwrap_or(false);
        let comma = comma.unwrap_or(true);

        nodes.iter().enumerate().for_each(|(i, node)| {
            match node {
                CodegenNode::Str(s) => self.push(s, None),
                CodegenNode::TemplateChilds(a) => {
                    let array = a
                        .iter()
                        .map(|n| CodegenNode::TemplateChild(n.clone()))
                        .collect();
                    self.gen_node_list_as_array(array);
                }
                x => {
                    self.gen_node(x);
                }
            }
            if i < nodes.len() - 1 {
                if multilines {
                    if comma {
                        self.push(",", None);
                    }
                    self.newline(None);
                } else if comma {
                    self.push(", ", None);
                }
            }
        })
    }

    fn gen_node_list_as_array(&mut self, nodes: Vec<CodegenNode>) {
        let multilines = nodes.len() > 3
            || ((!__BROWSER__ || __DEV__)
                && nodes.iter().any(|n| {
                    matches!(
                        n,
                        CodegenNode::TemplateChilds(_)
                            | CodegenNode::JSChild(Expr(_))
                            | CodegenNode::TemplateChild(TemplateChildNode::Text(_))
                            | CodegenNode::TemplateChild(TemplateChildNode::Interpolation(_))
                    )
                }));

        self.push("[", None);
        if multilines {
            self.indent();
        }
        self.gen_node_list(nodes, Some(multilines), None);
        if multilines {
            self.deindent(false);
        }
        self.push("]", None);
    }
}

/// a temp struct
pub struct PushedNode<'a> {
    pub kind: NodeType,
    pub is_static: bool,
    pub content: &'a str,
    pub loc: &'a SourceLocation,
}

#[inline]
fn alias_helper(s: &RuntimeHelper) -> String {
    format!("{x}: _${x}", x = s.into_str())
}
