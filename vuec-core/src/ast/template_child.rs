use crate::ast::{el::ElementNode, expr::Expr, utils::SourceLocation, Node, NodeType};

/// ElementNode | InterpolationNode | CompoundExprNode | TextNode| CommentNode| IfNode|
/// IfBranchNode| ForNode|TextCallNode
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TemplateChildNode {
    Element(ElementNode),
    Interpolation(Node<Interpolation>),
    CompoundExpr,
    Text(Node<Text>),
    Comment(Node<Comment>),
    If,
    IfBranch,
    For,
    TextCall,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Interpolation {
    pub content: Node<Expr>,
}

impl Node<Interpolation> {
    pub fn new(content: Node<Expr>, loc: SourceLocation) -> Self {
        Self {
            kind: NodeType::Interpolation,
            loc,
            inner: Interpolation { content },
        }
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

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Comment {
    content: String,
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
            TemplateChildNode::CompoundExpr => unimplemented!(),
            TemplateChildNode::Text(node) => &node.loc,
            TemplateChildNode::Comment(node) => &node.loc,
            TemplateChildNode::If => unimplemented!(),
            TemplateChildNode::IfBranch => unimplemented!(),
            TemplateChildNode::For => unimplemented!(),
            TemplateChildNode::TextCall => unimplemented!(),
        }
    }
}
