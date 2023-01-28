use std::rc::Rc;

use crate::{
    ast::{
        codegen::{DirArgs, SSRCodegenNode},
        js_child::{JSChildNode, Prop},
        stmt::{BlockStmt, IfStmt},
        template_child::{Interpolation, TemplateChildNode, Text},
        ConstantType, Node, NodeType, SourceLocation,
    },
    runtime_helpers::RuntimeHelper,
};

/// SimpleExpressionNode | CompoundExpressionNode
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ExprNode {
    Simple(Node<SimpleExpr>),
    Compound(Node<CompoundExpr>),
}

impl Default for ExprNode {
    fn default() -> Self {
        todo!()
    }
}

impl ExprNode {
    pub fn new_simple_expr(
        content: String,
        is_static: bool,
        const_type: ConstantType,
        loc: SourceLocation,
    ) -> ExprNode {
        Self::Simple(Node::<SimpleExpr>::new(content, is_static, const_type, loc))
    }
    pub fn identifiers(&self) -> &Option<Vec<String>> {
        match self {
            ExprNode::Simple(simple) => &simple.inner.identifiers,
            ExprNode::Compound(_compound) => unimplemented!(),
        }
    }
    pub fn loc(&self) -> &SourceLocation {
        match self {
            ExprNode::Simple(simple) => &simple.loc,
            ExprNode::Compound(compound) => &compound.loc,
        }
    }
    pub fn kind(&self) -> NodeType {
        match self {
            ExprNode::Simple(_simple) => NodeType::SimpleExpr,
            ExprNode::Compound(_compound) => NodeType::CompoundExpr,
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct SimpleExpr {
    pub content: String,
    pub is_static: bool,
    pub const_type: ConstantType,
    /// Indicates this is an identifier for a hoist vnode call and points to the
    /// hoisted node.
    pub hoisted: Option<Rc<JSChildNode>>,
    /// an expression parsed as the params of a function will track
    /// the identifiers declared inside the function body.
    pub identifiers: Option<Vec<String>>,
    pub is_handler_key: bool,
}

impl Eq for SimpleExpr {}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct CompoundExpr {
    pub children: Vec<Node<CompoundChildNode>>,
}

impl Node<SimpleExpr> {
    pub fn new(
        content: String,
        is_static: bool,
        const_type: ConstantType,
        loc: SourceLocation,
    ) -> Self {
        Self {
            kind: NodeType::SimpleExpr,
            loc,
            inner: SimpleExpr {
                content,
                is_static,
                const_type,
                hoisted: None,
                identifiers: None,
                is_handler_key: false,
            },
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum CompoundChildNode {
    Expr(ExprNode),
    Interpolation(Node<Interpolation>),
    Text(Node<Text>),
    Str(String),
    RH(RuntimeHelper),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum CallExprArgsNode {
    Str(String),
    RH(RuntimeHelper),
    JSChild(JSChildNode),
    SSR(SSRCodegenNode),
    TemplateChild(TemplateChildNode),
    TemplateChilds(Vec<TemplateChildNode>),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum CalleeNode {
    Str(String),
    RH(RuntimeHelper),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct CallExpr {
    pub callee: CalleeNode,
    pub args: Vec<CallExprArgsNode>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct CacheExpr {
    pub index: usize,
    pub value: JSChildNode,
    pub is_vnode: bool,
}

impl Node<CacheExpr> {
    pub fn new(index: usize, value: JSChildNode, is_vnode: bool) -> Self {
        Self {
            kind: NodeType::JSCacheExpr,
            loc: Default::default(),
            inner: CacheExpr {
                index,
                value,
                is_vnode,
            },
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct CondExpr {
    pub test: JSChildNode,
    pub consequent: JSChildNode,
    pub alternate: JSChildNode,
    pub newline: bool,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ConsequentNode {
    BlockCodegen,
    MemoExpr,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum AlternateNode {
    BlockCodegen,
    IfCondExpr(Box<Node<IfCondExpr>>),
    MemoExpr,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct IfCondExpr {
    test: JSChildNode,
    consequent: ConsequentNode,
    alternate: AlternateNode,
    newline: bool,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ObjExpr {
    pub properties: Vec<Node<Prop>>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum PropsExprNode {
    ObjectExpr(Node<ObjExpr>),
    CallExpr(Node<CallExpr>),
    Expr(ExprNode),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct AssignmentExpr {
    pub left: Node<SimpleExpr>,
    pub right: JSChildNode,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct SequenceExpr {
    pub exprs: Vec<JSChildNode>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ArrayExprNode {
    DirArgs(Node<DirArgs>),
    DynSlotEntries(Node<DirArgs>),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum FuncArgsNode {
    Expr(ExprNode),
    Str(String),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum FuncReturns {
    TemplateChild(Vec<TemplateChildNode>),
    JSChild(JSChildNode),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum FuncBodyNode {
    BlockStmt(Node<BlockStmt>),
    IfStmt(Node<IfStmt>),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct FuncExpr {
    pub params: Vec<FuncArgsNode>,
    pub returns: Option<FuncReturns>,
    pub body: Option<FuncBodyNode>,
    pub newline: bool,
    /// This flag is for codegen to determine whether it needs to generate the
    /// withScopeId() wrapper
    pub is_slot: bool,
    /// __COMPAT__ only, indicates a slot function that should be excluded from
    /// the legacy $scopedSlots instance property.
    pub is_non_scoped_slot: bool,
}
