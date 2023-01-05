use crate::ast::Node;
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BlockStmt {
    body: Vec<Node<BlockBody>>,
}
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum BlockBody {
    JSChild,
    IfStmt,
}
