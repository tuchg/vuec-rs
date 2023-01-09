use std::collections::HashSet;

use crate::{
    ast::{
        js_child::JSChildNode,
        template_child::TemplateChildNode,
        utils::{SourceLocation, LOC_STUB},
        Node, NodeType,
    },
    runtime_helpers::RuntimeHelper,
    transforms::ImportItem,
};

/// RootNode | ElementNode | IfBranchNode | ForNode\
pub trait ParentNode {
    fn children(&self) -> &Vec<TemplateChildNode>;
}

impl ParentNode for Node<Root> {
    fn children(&self) -> &Vec<TemplateChildNode> {
        &self.inner.children
    }
}

#[derive(Clone, Debug, Eq)]
pub struct Root {
    pub cached: usize,
    pub temps: usize,
    pub components: Vec<String>,
    pub directives: Vec<String>,
    pub hoists: Vec<JSChildNode>,
    pub imports: Vec<ImportItem>,
    pub children: Vec<TemplateChildNode>,
    pub helpers: HashSet<RuntimeHelper>,

    pub ssr_helpers: Option<Vec<RuntimeHelper>>,
    pub codegen_node: Option<Node<RootCodegen>>,

    // v2 compat only
    pub filters: Option<Vec<String>>,
}

impl PartialEq for Root {
    fn eq(&self, other: &Self) -> bool {
        todo!()
    }
}

impl Node<Root> {
    pub fn new(children: Vec<TemplateChildNode>, loc: Option<SourceLocation>) -> Self {
        let loc = loc.unwrap_or(LOC_STUB);
        Self {
            kind: NodeType::Root,
            loc,
            inner: Root {
                children,
                helpers: Default::default(),
                components: vec![],
                directives: vec![],
                hoists: Default::default(),
                imports: Default::default(),
                cached: 0,
                temps: 0,
                ssr_helpers: None,
                codegen_node: None,
                filters: None,
            },
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum RootCodegen {
    TemplateChild,
    JSChild,
    BlockStmt,
}
