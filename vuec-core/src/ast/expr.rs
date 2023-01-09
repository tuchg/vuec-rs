use crate::ast::{js_child::JSChildNode, ConstantType, Node, NodeType, SourceLocation};

/// SimpleExpressionNode | CompoundExpressionNode
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Expr {
    Simple(SimpleExpr),
    Compound(CompoundExpr),
}

#[derive(Clone, PartialEq, Debug)]
pub struct SimpleExpr {
    pub content: String,
    pub is_static: bool,
    pub const_type: ConstantType,
    /// Indicates this is an identifier for a hoist vnode call and points to the
    /// hoisted node.
    pub hoisted: Option<Node<JSChildNode>>,
    /// an expression parsed as the params of a function will track
    /// the identifiers declared inside the function body.
    pub identifiers: Option<Vec<String>>,
    pub is_handler_key: bool,
}

impl Eq for SimpleExpr {}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct CompoundExpr {
    pub children: Vec<Node<CompoundChild>>,
}

impl Node<Expr> {
    pub fn new_simple_expr(
        content: String,
        is_static: bool,
        const_type: ConstantType,
        loc: SourceLocation,
    ) -> Self {
        Self {
            kind: NodeType::SimpleExpr,
            loc,
            inner: Expr::Simple(SimpleExpr {
                content,
                is_static,
                const_type,
                hoisted: None,
                identifiers: None,
                is_handler_key: false,
            }),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum CompoundChild {
    SimpleExpr,
    CompoundExpr,
    Interpolation,
    Text,
    String,
}

#[derive(Clone, PartialEq, Eq, Debug)]
enum CallExprArgs {
    String,
    JSChildNode,
    SSRCodegenNode,
    TemplateChildNode,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct CallExpr {
    callee: String,
    arguments: Vec<Node<CallExprArgs>>,
}
