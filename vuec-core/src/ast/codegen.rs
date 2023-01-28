use crate::{
    ast::{
        el::TemplateLit,
        expr::{AssignmentExpr, ExprNode, ObjExpr, SequenceExpr},
        js_child::JSChildNode,
        stmt::{BlockStmt, IfStmt, ReturnStmt},
        template_child::{IfCodegenNode, TemplateChildNode},
        utils::SourceLocation,
        Node, NodeType,
    },
    runtime_helpers::RuntimeHelper,
};

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum CodegenNode {
    TemplateChild(TemplateChildNode),
    TemplateChilds(Vec<TemplateChildNode>),
    JSChild(JSChildNode),
    // BlockStmt,
    SSR(SSRCodegenNode),
    RH(RuntimeHelper),
    Str(String),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum SSRCodegenNode {
    BlockStmt(Node<BlockStmt>),
    TemplateLiteral(Node<TemplateLit>),
    IfStmt(Node<IfStmt>),
    AssignmentExpr(Node<AssignmentExpr>),
    ReturnStmt(Node<ReturnStmt>),
    SeqExpr(Node<SequenceExpr>),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct DirArgs {
    pub elements: Vec<DirArgNode>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum DirArgNode {
    Dir(String),
    Expr(String, ExprNode),
    Arg(String, ExprNode, ExprNode),
    Modifiers(String, ExprNode, ExprNode, Node<ObjExpr>),
}

impl CodegenNode {
    pub fn kind(&self) -> NodeType {
        match self {
            CodegenNode::TemplateChild(node) => node.kind(),
            CodegenNode::JSChild(node) => node.kind(),
            CodegenNode::Str(_) => unimplemented!(),
            CodegenNode::RH(_) => unimplemented!(),
            _ => unimplemented!(),
        }
    }
    pub fn loc(&self) -> &SourceLocation {
        match self {
            CodegenNode::TemplateChild(node) => node.loc(),
            CodegenNode::JSChild(node) => node.loc(),
            CodegenNode::Str(_) => unimplemented!(),
            CodegenNode::RH(_) => unimplemented!(),
            _ => unimplemented!(),
        }
    }
}
