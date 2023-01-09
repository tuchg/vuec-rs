#[derive(Clone, PartialEq, Eq, Debug)]
pub enum JSChildNode {
    VNodeCall,
    CallExpr,
    ObjectExpr,
    ArrayExpr,
    ExprNode,
    FunctionExpr,
    ConditionalExpr,
    CacheExpr,
    AssignmentExpr,
    SequenceExpr,
}
