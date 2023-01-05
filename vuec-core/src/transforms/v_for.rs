use crate::ast::{expr::Expr, Node};

#[derive(Clone, PartialEq, Debug, Eq)]
pub struct ForParseResult {
    source: Node<Expr>,
    value: Option<Node<Expr>>,
    key: Option<Node<Expr>>,
    index: Option<Node<Expr>>,
}
