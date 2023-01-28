use crate::ast::expr::ExprNode;

#[derive(Clone, PartialEq, Debug, Eq)]
pub struct ForParseResult {
    source: ExprNode,
    value: Option<ExprNode>,
    key: Option<ExprNode>,
    index: Option<ExprNode>,
}
