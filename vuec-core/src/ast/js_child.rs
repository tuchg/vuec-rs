use crate::{
    ast::{
        codegen::DirArgs,
        expr::{
            AssignmentExpr, CacheExpr, CallExpr, CondExpr, ExprNode, FuncExpr, ObjExpr,
            PropsExprNode, SequenceExpr, SimpleExpr,
        },
        template_child::TemplateChildNode,
        utils::SourceLocation,
        Node, NodeType,
    },
    shared::flags::PatchFlags,
};

// JS Node Types ---------------------------------------------------------------

// We also include a number of JavaScript AST nodes for code generation.
// The AST is an intentionally minimal subset just to meet the exact needs of
// Vue render function generation.

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum JSChildNode {
    VNodeCall(Box<Node<VNodeCall>>),
    CallExpr(Box<Node<CallExpr>>),
    ObjExpr(Box<Node<ObjExpr>>),
    ArrayExpr(Box<Node<DirArgs>>),
    Expr(Box<ExprNode>),
    FuncExpr(Box<Node<FuncExpr>>),
    CondExpr(Box<Node<CondExpr>>),
    CacheExpr(Box<Node<CacheExpr>>),
    AssignmentExpr(Box<Node<AssignmentExpr>>),
    SequenceExpr(Box<Node<SequenceExpr>>),
    Str(String),
}

impl JSChildNode {
    pub fn loc(&self) -> &SourceLocation {
        match self {
            JSChildNode::VNodeCall(node) => &node.loc,
            JSChildNode::CallExpr(node) => &node.loc,
            JSChildNode::ObjExpr(node) => &node.loc,
            JSChildNode::ArrayExpr(node) => &node.loc,
            JSChildNode::Expr(node) => node.loc(),
            JSChildNode::FuncExpr(node) => &node.loc,
            JSChildNode::CondExpr(node) => &node.loc,
            JSChildNode::CacheExpr(node) => &node.loc,
            JSChildNode::AssignmentExpr(node) => &node.loc,
            JSChildNode::SequenceExpr(node) => &node.loc,
            JSChildNode::Str(_) => unimplemented!(),
        }
    }
    pub fn kind(&self) -> NodeType {
        todo!()
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Prop {
    pub key: ExprNode,
    pub value: JSChildNode,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct VNodeCall {
    pub is_block: bool,
    pub disable_tracking: bool,
    pub is_component: bool,
    pub tag: CallExpr,
    pub props: Option<PropsExprNode>,
    pub patch_flag: Option<PatchFlags>,
    pub dynamic_props: Option<Node<SimpleExpr>>,
    pub directives: Option<Node<DirArgs>>,
    pub children: Option<TemplateChildNode>,
}
