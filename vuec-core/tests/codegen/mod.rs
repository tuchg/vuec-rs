use std::collections::HashSet;

use vuec_core::{
    ast::{
        codegen::CodegenNode,
        expr::{ExprNode, SimpleExpr},
        js_child::JSChildNode,
        parent::Root,
        utils::LOC_STUB,
        Node, NodeType,
    },
    codegen::generate,
    options::{CodegenMode, CodegenOptions},
    runtime_helpers::RuntimeHelper,
};

mod if_stmt;
mod vnode_call;

fn root() -> Root {
    Root {
        cached: 0,
        temps: 0,
        components: vec![],
        directives: vec![],
        hoists: vec![],
        imports: vec![],
        children: vec![],
        helpers: Default::default(),
        ssr_helpers: None,
        codegen_node: Some(CodegenNode::JSChild(JSChildNode::Expr(Box::from(
            ExprNode::new_simple_expr(
                "null".to_string(),
                false,
                Default::default(),
                Default::default(),
            ),
        )))),
        source: "".to_string(),
        filters: None,
    }
}

#[test]
fn module_preamble() {
    let ast = Node::<Root> {
        kind: NodeType::Root,
        loc: LOC_STUB,
        inner: Root {
            helpers: HashSet::from([RuntimeHelper::CreateVNode, RuntimeHelper::ResolveDirective]),
            ..root()
        },
    };
    let result = generate(
        ast,
        Some(CodegenOptions {
            mode: CodegenMode::Module,
            ..Default::default()
        }),
        Default::default(),
    );
    assert_eq!(
        result.code,
        format!(
            r#"import {{ {x} as _{x}, {y} as _{y} }} from "vue""#,
            x = RuntimeHelper::CreateVNode.into_str(),
            y = RuntimeHelper::ResolveDirective.into_str(),
        )
    )
}

#[test]
/// module mode preamble w/ optimizeImports: true
fn module_preamble_with_opt_imports() {}

#[test]
fn function_preamble() {}

#[test]
/// function mode preamble w/ prefixIdentifiers: true
fn function_preamble_with_opt_identifiers() {}

#[test]
fn assets_and_temps() {}

#[test]
fn hoists() {}

#[test]
fn temps() {}

#[test]
fn static_text() {}

#[test]
fn interpolation() {}

#[test]
fn comment() {}

#[test]
fn compound_expr() {}

#[test]
fn if_node() {}

#[test]
fn for_node() {}

#[test]
fn for_node_with_const_expr() {}

#[test]
/// Element (callExpression + objectExpression + TemplateChildNode[])
fn element() {}

#[test]
fn array_expr() {}

#[test]
fn cond_expr() {}

#[test]
fn cache_expr() {}

#[test]
fn cache_expr_with_vnode() {}

#[test]
fn template_lit() {}
