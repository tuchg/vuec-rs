use crate::ast::{expr::ExprNode, js_child::JSChildNode, template_child::TemplateChildNode, Node};

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BlockStmt {
    pub body: Vec<Node<BlockBody>>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum BlockBody {
    JSChild(JSChildNode),
    IfStmt(Node<IfStmt>),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum IfStmtAltNode {
    IfStmt(Node<IfStmt>),
    BlockStmt(Node<BlockStmt>),
    ReturnStmt(Node<ReturnStmt>),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct IfStmt {
    pub test: ExprNode,
    pub consequent: Node<BlockStmt>,
    pub alternate: Option<Box<IfStmtAltNode>>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ReturnsNode {
    TemplateChild(Vec<TemplateChildNode>),
    JSChild(JSChildNode),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ReturnStmt {
    pub returns: ReturnsNode,
}
