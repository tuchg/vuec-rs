use crate::{
    ast::{expr::Expr, template_child::Text, utils::SourceLocation, Node, NodeType},
    transforms::v_for::ForParseResult,
};

#[derive(Clone, PartialEq, Debug, Eq)]
pub enum AttrsNode {
    Attr(Node<Attribute>),
    Dir(Box<Node<Directive>>),
}

impl AttrsNode {
    pub fn new_attr(name: String, value: Option<Node<Text>>, loc: SourceLocation) -> Self {
        Self::Attr(Node::<Attribute>::new(name, value, loc))
    }

    pub fn new_dir(
        name: String,
        arg: Option<Node<Expr>>,
        expr: Option<Node<Expr>>,
        modifiers: Vec<String>,
        loc: SourceLocation,
    ) -> Self {
        Self::Dir(Box::from(Node::<Directive>::new(
            name, arg, expr, modifiers, loc,
        )))
    }

    pub fn name(&self) -> &String {
        match &self {
            AttrsNode::Attr(attr) => &attr.inner.name,
            AttrsNode::Dir(dir) => &dir.inner.name,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Attribute {
    pub name: String,
    pub value: Option<Node<Text>>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct AttributeValue {
    pub content: String,
    pub is_quoted: bool,
}

impl Node<AttributeValue> {
    pub fn new(content: String, is_quoted: bool, loc: SourceLocation) -> Self {
        Self {
            kind: NodeType::Attribute,
            loc,
            inner: AttributeValue { content, is_quoted },
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Directive {
    pub name: String,
    pub arg: Option<Node<Expr>>,
    pub expr: Option<Node<Expr>>,
    pub modifiers: Vec<String>,
    /// optional property to cache the expression parse result for v-for
    pub parse_result: Option<ForParseResult>,
}

impl Node<Attribute> {
    pub fn new(name: String, value: Option<Node<Text>>, loc: SourceLocation) -> Self {
        Self {
            kind: NodeType::Attribute,
            loc,
            inner: Attribute { name, value },
        }
    }
}

impl Node<Directive> {
    pub fn new(
        name: String,
        arg: Option<Node<Expr>>,
        expr: Option<Node<Expr>>,
        modifiers: Vec<String>,
        loc: SourceLocation,
    ) -> Self {
        Self {
            kind: NodeType::Directive,
            loc,
            inner: Directive {
                name,
                expr,
                arg,
                modifiers,
                parse_result: None,
            },
        }
    }
}
