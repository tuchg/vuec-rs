#[derive(Clone, PartialEq, Eq, Debug)]
pub enum JSChild {
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
