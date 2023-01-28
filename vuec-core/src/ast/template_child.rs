use crate::ast::{
    attr::AttrsNode,
    codegen::CodegenNode,
    el::ElementNode,
    expr::{CacheExpr, CompoundExpr, ExprNode, IfCondExpr},
    utils::SourceLocation,
    Node, NodeType,
};

/// ElementNode | InterpolationNode | CompoundExprNode | TextNode| CommentNode| IfNode|
/// IfBranchNode| ForNode|TextCallNode
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TemplateChildNode {
    Element(ElementNode),
    Interpolation(Node<Interpolation>),
    CompoundExpr(Node<CompoundExpr>),
    Text(Node<Text>),
    Comment(Node<Comment>),
    If(Node<If>),
    IfBranch(Node<IfBranch>),
    For,
    TextCall,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct IfBranch {
    is_template_if: bool,
    cond: Option<ExprNode>,
    children: Vec<TemplateChildNode>,
    user_key: AttrsNode,
}

/// <div v-if v-once>
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum IfCodegenNode {
    IfCond(Node<IfCondExpr>),
    CacheExpr(Node<CacheExpr>),
}

impl Into<CodegenNode> for IfCodegenNode {
    fn into(self) -> CodegenNode {
        match self {
            IfCodegenNode::IfCond(node) => unimplemented!(),
            IfCodegenNode::CacheExpr(node) => unimplemented!(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct If {
    pub branches: Vec<Node<IfBranch>>,
    pub codegen: IfCodegenNode,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Interpolation {
    pub content: ExprNode,
}

impl Node<Interpolation> {
    pub fn new(content: ExprNode, loc: SourceLocation) -> Self {
        Self {
            kind: NodeType::Interpolation,
            loc,
            inner: Interpolation { content },
        }
    }
    pub fn loc(&self) -> &SourceLocation {
        &self.loc
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Text {
    pub content: String,
}

impl Eq for Text {}

impl Node<Text> {
    pub fn new(content: String, loc: SourceLocation) -> Self {
        Self {
            kind: NodeType::Text,
            loc,
            inner: Text { content },
        }
    }
}

impl Default for Node<Text> {
    fn default() -> Self {
        Self {
            kind: NodeType::Text,
            loc: SourceLocation::default(),
            inner: Text {
                content: String::default(),
            },
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Comment {
    pub content: String,
}

impl Node<Comment> {
    pub fn new(content: String, loc: SourceLocation) -> Self {
        Self {
            kind: NodeType::Comment,
            loc,
            inner: Comment { content },
        }
    }
}

impl TemplateChildNode {
    pub fn kind(&self) -> NodeType {
        match self {
            TemplateChildNode::Element(_) => NodeType::Element,
            TemplateChildNode::Interpolation(_) => NodeType::Interpolation,
            TemplateChildNode::CompoundExpr(_) => NodeType::CompoundExpr,
            TemplateChildNode::Text(_) => NodeType::Text,
            TemplateChildNode::Comment(_) => NodeType::Comment,
            TemplateChildNode::If(_) => NodeType::If,
            TemplateChildNode::IfBranch(_) => NodeType::IfBranch,
            TemplateChildNode::For => NodeType::For,
            TemplateChildNode::TextCall => NodeType::TextCall,
        }
    }
    pub fn new_el(node: ElementNode) -> Self {
        TemplateChildNode::Element(node)
    }
    pub fn new_interpolation(node: Node<Interpolation>) -> Self {
        TemplateChildNode::Interpolation(node)
    }
    pub fn new_text(node: Node<Text>) -> Self {
        TemplateChildNode::Text(node)
    }
    pub fn new_comment(node: Node<Comment>) -> Self {
        TemplateChildNode::Comment(node)
    }

    pub fn loc(&self) -> &SourceLocation {
        match self {
            TemplateChildNode::Element(node) => node.loc(),
            TemplateChildNode::Interpolation(node) => &node.loc,
            TemplateChildNode::CompoundExpr(_) => unimplemented!(),
            TemplateChildNode::Text(node) => &node.loc,
            TemplateChildNode::Comment(node) => &node.loc,
            TemplateChildNode::If(node) => &node.loc,
            TemplateChildNode::IfBranch(node) => &node.loc,
            TemplateChildNode::For => unimplemented!(),
            TemplateChildNode::TextCall => unimplemented!(),
        }
    }
}
