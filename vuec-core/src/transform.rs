use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

use regex::Regex;

use self::CurrentNode::*;
use crate::{
    ast::{
        attr::Directive,
        el::{ElementNode, TemplateLit},
        expr::{CacheExpr, ExprNode, SimpleExpr},
        js_child::{JSChildNode, Prop},
        parent::{ParentNode, Root},
        template_child::{TemplateChildNode, TemplateChildNode::*, Text},
        ConstantType, Node, NodeType,
    },
    options::TransformOptions,
    runtime_helpers::RuntimeHelper,
    __BROWSER__, __COMPAT__, __DEV__,
};

struct Scopes {
    v_for: usize,
    v_slot: usize,
    v_pre: usize,
    v_once: usize,
}

/// NodeTransform:
///   Transforms that operate directly on a ChildNode. NodeTransforms may mutate,
///   replace or remove the node being processed.
pub type NodeTransform = fn(node: CurrentNode) -> Vec<fn()>;

/// DirectiveTransform:
///   Transforms that handles a single directive attribute on an element.
///   It translates the raw directive into actual props for the VNode.
pub type DirectiveTransform = fn(
    dir: Node<Directive>,
    node: ElementNode,
    context: TransformContext,
    // a platform specific compiler can import the base transform and augment
    // it by passing in this optional argument.
    augmentor: Option<fn(ret: DirectiveTransformResult) -> DirectiveTransformResult>,
) -> DirectiveTransformResult;

pub struct DirectiveTransformResult {
    props: Vec<Node<Prop>>,
    need_runtime: bool,
    ssr_tag_parts: TemplateLit,
}

/// A structural directive transform is technically also a NodeTransform;
/// Only v-if and v-for fall into this category.
pub type StructureDirectiveTransform =
    fn(node: ElementNode, dir: Node<Directive>, context: TransformContext) -> fn();

pub type HoistTransform<T> =
    fn(children: Vec<TemplateChildNode>, context: TransformContext, parent: T);

#[derive(Clone, PartialEq, Debug, Eq)]
pub struct ImportItem {
    pub expr: ExprNode,
    pub path: String,
}

#[derive(Clone, PartialEq, Debug, Eq)]
pub enum CurrentNode {
    Root(Rc<RefCell<Node<Root>>>),
    Child(TemplateChildNode),
}

impl CurrentNode {
    fn kind(&self) -> NodeType {
        match self {
            Root(node) => NodeType::Root,
            Child(node) => node.kind(),
        }
    }
    fn into_parent(&self) -> ParentNode {
        match self {
            Root(node) => unimplemented!(),
            Child(node) => match node {
                Element(el) => ParentNode::Element(el.clone()),
                _ => unimplemented!(),
            },
        }
    }
}

pub struct TransformContext<'a> {
    self_name: Option<String>,
    root: Rc<RefCell<Node<Root>>>,
    helpers: HashMap<RuntimeHelper, i32>,
    components: HashSet<&'a str>,
    directives: HashSet<&'a str>,
    hoists: Vec<&'a JSChildNode>,
    imports: Vec<ImportItem>,
    temps: usize,
    cached: usize,
    identifiers: HashMap<&'a str, i32>,
    scopes: Scopes,
    child_index: usize,
    parent: Option<ParentNode>,
    current_node: Option<CurrentNode>,
    in_v_once: bool,
    constant_cache: HashMap<TemplateChildNode, ConstantType>,
    options: TransformOptions,
    on_node_removed: &'a dyn FnMut(),

    // 2.x Compat only
    filters: Option<HashSet<&'a str>>,
}

/// string | JSChildNode | ArrayExpression
pub enum HoistExprNode {
    Str(String),
    JSChild(JSChildNode),
}

pub enum IdentifiersExprNode {
    Str(String),
    Expr(ExprNode),
}

pub trait TransformContextOps<'a> {
    fn helper(&mut self, name: RuntimeHelper) -> RuntimeHelper;
    fn remove_helper(&mut self, name: RuntimeHelper);
    fn helper_str(&mut self, name: RuntimeHelper) -> String;
    fn replace_node(&mut self, node: &TemplateChildNode);
    fn remove_node(&mut self, node: Option<TemplateChildNode>);
    fn add_identifiers(&mut self, expr: &'a IdentifiersExprNode);
    fn remove_identifiers(&mut self, expr: &'a IdentifiersExprNode);
    fn hoist(&mut self, expr: HoistExprNode) -> Node<SimpleExpr>;
    fn cache(&mut self, expr: JSChildNode, is_vnode: bool) -> Node<CacheExpr>;
}

pub struct Transform;

impl Transform {
    pub fn transform(root: Rc<RefCell<Node<Root>>>, options: Option<TransformOptions>) {
        let options = options.unwrap_or_default();
        let hoist_static = options.hoist_static;
        let codegen_ssr = options.codegen_options.ssr;
        let mut context = TransformContext::new(root.clone(), options);

        context.traverse_node(CurrentNode::Root(root.clone()));
        if hoist_static {
            context.hoist_static();
        }
        if !codegen_ssr {
            context.create_root_codegen();
        }
        // let helper = context.helpers.into_iter().map(|x| x.0).collect::<RuntimeHelper>();
        // root.helpers = helper;
        // root.components = context.components.into_iter().collect::<Vec<&str>>();
        // root.borrow_mut().inner.helpers = context.helpers.into_iter().map(|x| x.0).collect();
        let mut root = root.borrow_mut();
        root.inner.imports = context.imports;
        // root.hoists = context.hoists;
        root.inner.temps = context.temps;
        root.inner.cached = context.cached;

        if __COMPAT__ {
            // root.filters = Some(context.filters.into_iter().map(String::from).collect());
        }
    }
}

impl<'a> TransformContext<'a> {
    pub fn new(root: Rc<RefCell<Node<Root>>>, options: TransformOptions) -> Self {
        let filename = options.codegen_options.filename.clone();
        let replace = Regex::new(r"\?.*$").unwrap().replace(&filename, "");
        // todo: capitalize(camelize(
        let self_name = Regex::new(r"([^/\\]+)\.\w+$")
            .unwrap()
            .captures(&replace)
            .map(|x| x.get(1).unwrap().as_str().to_string());

        Self {
            current_node: Some(CurrentNode::Root(root.clone())),
            self_name,
            root,
            helpers: HashMap::new(),
            components: HashSet::new(),
            directives: HashSet::new(),
            hoists: vec![],
            imports: vec![],
            temps: 0,
            cached: 0,
            identifiers: HashMap::new(),
            scopes: Scopes {
                v_for: 0,
                v_slot: 0,
                v_pre: 0,
                v_once: 0,
            },
            child_index: 0,
            parent: None,
            in_v_once: false,
            constant_cache: HashMap::new(),
            options,
            on_node_removed: &|| {},
            filters: if __COMPAT__ {
                Some(HashSet::new())
            } else {
                None
            },
        }
    }

    pub fn traverse_node(&mut self, node: CurrentNode) {
        self.current_node = Some(node.clone());
        let mut node = node;
        // apply transform plugins
        let mut exit_fns = Vec::with_capacity(self.options.node_transforms.len());
        for transform in &mut self.options.node_transforms {
            let on_exits = transform(node);
            exit_fns.extend(on_exits);

            if let Some(current_node) = &self.current_node {
                // node may have been replaced
                node = current_node.clone();
            } else {
                // node was removed
                return;
            }
        }

        use NodeType::*;
        match node.kind() {
            Comment => {
                if !self.options.codegen_options.ssr {
                    // inject import for the Comment symbol, which is needed for creating
                    // comment nodes with `createVNode`
                    self.helper(RuntimeHelper::CreateComment);
                }
            }
            Interpolation => {
                if !self.options.codegen_options.ssr {
                    // no need to traverse, but we need to inject toString helper
                    self.helper(RuntimeHelper::ToDisplayString);
                }
            }
            // for container types, further traverse downwards
            If => {
                if let Child(TemplateChildNode::If(mut node)) = node.clone() {
                    node.inner.branches.iter_mut().for_each(|branch| {
                        self.traverse_node(Child(TemplateChildNode::IfBranch(branch.clone())));
                    });
                }
            }
            IfBranch | For | Element | Root => {
                self.traverse_children(node.into_parent());
            }
            _ => {}
        }
        // exit transforms
        self.current_node = Some(node);
        exit_fns.iter().rev().for_each(|exit| exit());
    }

    pub fn traverse_children(&mut self, parent: ParentNode) {
        let mut i = 0;
        let children = parent.children();
        let on_removed = move || i -= 1;
        while i < children.len() {
            let child = &children[i];
            // TODO: isString?
            // self.parent = Some(parent);
            self.child_index = i;
            // self.on_node_removed = &on_removed;
            self.traverse_node(Child(child.clone()));
            i += 1;
        }
    }

    pub fn create_root_codegen(&mut self) {}

    fn add_id(&mut self, id: &'a str) {
        self.identifiers
            .insert(id, self.identifiers.get(id).unwrap_or(&0) + 1);
    }
    fn remove_id(&mut self, id: &'a str) {
        self.identifiers
            .insert(id, self.identifiers.get(id).unwrap_or(&0) - 1);
    }
}

impl<'a> TransformContextOps<'a> for TransformContext<'a> {
    fn helper(&mut self, name: RuntimeHelper) -> RuntimeHelper {
        let cnt = self.helpers.entry(name).or_insert(0);
        *cnt += 1;
        name
    }

    fn remove_helper(&mut self, name: RuntimeHelper) {
        let cnt = self.helpers.get(&name);
        if let Some(cnt) = cnt {
            let curr_cnt = cnt - 1;
            if curr_cnt >= 0 {
                self.helpers.remove(&name);
            } else {
                self.helpers.insert(name, curr_cnt);
            }
        }
    }

    fn helper_str(&mut self, name: RuntimeHelper) -> String {
        format!("_{}", self.helper(name).into_str())
    }

    fn replace_node(&mut self, node: &TemplateChildNode) {
        // istanbul ignore if
        if __DEV__ {
            if self.current_node.is_none() {
                panic!("Node being replaced is already removed.")
            }

            if self.parent.is_none() {
                panic!("Cannot replace root node.")
            }
        }
        // self.current_node = Some(Child(&node));
        // self.parent.as_mut().unwrap().children_mut()[self.child_index] = node;
    }

    fn remove_node(&mut self, node: Option<TemplateChildNode>) {
        if let Some(parent) = &mut self.parent {
            let list = parent.children_mut();

            let removal_index = if let Some(node) = &node {
                list.iter().position(|n| n == node)
            } else {
                self.current_node.as_mut().map(|_| self.child_index)
            };

            if let Some(removal_index) = removal_index {
                // todo
                list.swap_remove(removal_index);

                // if let Some(node) = &node {
                //     if let Some(CurrentNode::Child(current_node)) = self.current_node && node ==
                // current_node {         self.current_node = None;
                //         self.on_node_removed();
                //     } else {
                //         // sibling node removed
                //         if self.child_index > removal_index {
                //             self.child_index -= 1;
                //             self.on_node_removed();
                //         }
                //     }
                // }
                // todo
            } else if __DEV__ {
                // istanbul ignore if
                panic!("node being removed is not a child of current parent")
            }
        } else if __DEV__ {
            panic!("Cannot remove root node.")
        }
    }

    fn add_identifiers(&mut self, expr: &'a IdentifiersExprNode) {
        // identifier tracking only happens in non-browser builds.
        if !__BROWSER__ {
            match expr {
                IdentifiersExprNode::Str(s) => self.add_id(&s),
                IdentifiersExprNode::Expr(expr) => {
                    if let Some(ids) = expr.identifiers() {
                        for id in ids {
                            self.add_id(id)
                        }
                    } else if let ExprNode::Simple(expr) = expr {
                        self.add_id(&expr.inner.content)
                    }
                }
            }
        }
    }

    fn remove_identifiers(&mut self, expr: &'a IdentifiersExprNode) {
        if !__BROWSER__ {
            match expr {
                IdentifiersExprNode::Str(s) => self.remove_id(s.as_str()),
                IdentifiersExprNode::Expr(expr) => {
                    if let Some(ids) = expr.identifiers() {
                        for id in ids {
                            self.remove_id(id)
                        }
                    } else if let ExprNode::Simple(expr) = expr {
                        self.remove_id(&expr.inner.content)
                    }
                }
            }
        }
    }

    fn hoist(&mut self, expr: HoistExprNode) -> Node<SimpleExpr> {
        let mut expr = expr;
        if let HoistExprNode::Str(s) = expr {
            let node =
                Node::<SimpleExpr>::new(s, false, ConstantType::NotConstant, Default::default());

            expr = HoistExprNode::JSChild(JSChildNode::Expr(Box::from(ExprNode::Simple(node))));
        }
        if let HoistExprNode::JSChild(expr) = expr {
            // self.hoists.push(&Expr);
            let mut identifier = Node::<SimpleExpr>::new(
                format!("_hoisted_{}", self.hoists.len()),
                false,
                ConstantType::CanHoist,
                expr.loc().clone(),
            );
            identifier.inner.hoisted = Some(Rc::from(expr));
            identifier
        } else {
            unreachable!()
        }
    }

    fn cache(&mut self, expr: JSChildNode, is_vnode: bool) -> Node<CacheExpr> {
        self.cached += 1;
        Node::<CacheExpr>::new(self.cached, expr, is_vnode)
    }
}
