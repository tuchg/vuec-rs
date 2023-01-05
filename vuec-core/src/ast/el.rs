use std::fmt::Debug;

use crate::ast::{
    attr::AttrsNode, expr::CallExpr, js_child::JSChild, template_child::TemplateChildNode,
    ElementType, NameSpace, Node, NodeType, PhantomType, SourceLocation,
};

/// PlainElementNode | ComponentNode | SlotOutletNode | TemplateNode
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ElementNode {
    Plain(ElementNodeType<PlainElementCodegen, TemplateLit>),
    Component(ElementNodeType<ComponentCodegen, CallExpr>),
    SlotOutlet(ElementNodeType<SlotOutletCodegen, CallExpr>),
    /// TemplateNode is a container type that always gets compiled away
    Template(ElementNodeType<PhantomType, PhantomType>),
}

type ElementNodeType<C, S> = Node<Element<C, S>>;

/// BaseElementNode
#[derive(Clone, Debug, Eq)]
pub struct Element<C, S>
where
    C: Clone + PartialEq + Debug,
    S: Clone + PartialEq + Debug,
{
    pub ns: NameSpace,
    pub tag: String,
    pub tag_type: ElementType,
    pub is_self_closing: bool,
    pub props: Vec<AttrsNode>,
    pub children: Vec<TemplateChildNode>,
    pub codegen_node: Option<C>,
    pub ssr_codegen_node: Option<S>,
}

impl<C, S> PartialEq for Element<C, S>
where
    C: Clone + PartialEq + Debug,
    S: Clone + PartialEq + Debug,
{
    fn eq(&self, other: &Self) -> bool {
        self.ns == other.ns
            && self.tag == other.tag
            && self.tag_type == other.tag_type
            && self.is_self_closing == other.is_self_closing
            && self.props == other.props
            && self.children == other.children
            && self.codegen_node == other.codegen_node
            && self.ssr_codegen_node == other.ssr_codegen_node
    }
}

impl<C: Clone + Debug + PartialEq + Eq, S: Clone + Debug + PartialEq + Eq> Node<Element<C, S>> {
    pub fn new_el(
        tag_type: ElementType,
        ns: NameSpace,
        tag: String,
        is_self_closing: bool,
        loc: SourceLocation,
        props: Vec<AttrsNode>,
        children: Vec<TemplateChildNode>,
    ) -> Self {
        Self {
            kind: NodeType::Element,
            loc,
            inner: Element {
                ns,
                tag,
                tag_type,
                is_self_closing,
                props,
                children,
                codegen_node: None,
                ssr_codegen_node: None,
            },
        }
    }
}

impl ElementNode {
    pub fn new(
        tag_type: ElementType,
        ns: NameSpace,
        tag: String,
        is_self_closing: bool,
        props: Vec<AttrsNode>,
        children: Vec<TemplateChildNode>,
        loc: SourceLocation,
    ) -> Self {
        match tag_type {
            ElementType::Element => ElementNode::Plain(Node::new_el(
                tag_type,
                ns,
                tag,
                is_self_closing,
                loc,
                props,
                children,
            )),
            ElementType::Component => ElementNode::Component(Node::new_el(
                tag_type,
                ns,
                tag,
                is_self_closing,
                loc,
                props,
                children,
            )),
            ElementType::Slot => ElementNode::SlotOutlet(Node::new_el(
                tag_type,
                ns,
                tag,
                is_self_closing,
                loc,
                props,
                children,
            )),
            ElementType::Template => ElementNode::Template(Node::new_el(
                tag_type,
                ns,
                tag,
                is_self_closing,
                loc,
                props,
                children,
            )),
        }
    }

    pub fn tag(&self) -> &String {
        match &self {
            ElementNode::Plain(el) => &el.inner.tag,
            ElementNode::Component(el) => &el.inner.tag,
            ElementNode::SlotOutlet(el) => &el.inner.tag,
            ElementNode::Template(el) => &el.inner.tag,
        }
    }

    pub fn loc(&self) -> &SourceLocation {
        match &self {
            ElementNode::Plain(el) => &el.loc,
            ElementNode::Component(el) => &el.loc,
            ElementNode::SlotOutlet(el) => &el.loc,
            ElementNode::Template(el) => &el.loc,
        }
    }

    pub fn loc_mut(&mut self) -> SourceLocation {
        self.loc().clone()
    }

    pub fn set_loc(&mut self, loc: SourceLocation) {
        match self {
            ElementNode::Plain(el) => el.loc = loc,
            ElementNode::Component(el) => el.loc = loc,
            ElementNode::SlotOutlet(el) => el.loc = loc,
            ElementNode::Template(el) => el.loc = loc,
        }
    }

    pub fn is_self_closing(&self) -> bool {
        match &self {
            ElementNode::Plain(el) => el.inner.is_self_closing,
            ElementNode::Component(el) => el.inner.is_self_closing,
            ElementNode::SlotOutlet(el) => el.inner.is_self_closing,
            ElementNode::Template(el) => el.inner.is_self_closing,
        }
    }
    pub fn children_mut(&mut self) -> &mut Vec<TemplateChildNode> {
        match self {
            ElementNode::Plain(el) => &mut el.inner.children,
            ElementNode::Component(el) => &mut el.inner.children,
            ElementNode::SlotOutlet(el) => &mut el.inner.children,
            ElementNode::Template(el) => &mut el.inner.children,
        }
    }
    pub fn children(&self) -> &Vec<TemplateChildNode> {
        match self {
            ElementNode::Plain(el) => &el.inner.children,
            ElementNode::Component(el) => &el.inner.children,
            ElementNode::SlotOutlet(el) => &el.inner.children,
            ElementNode::Template(el) => &el.inner.children,
        }
    }
    pub fn ns(&self) -> NameSpace {
        match &self {
            ElementNode::Plain(el) => el.inner.ns,
            ElementNode::Component(el) => el.inner.ns,
            ElementNode::SlotOutlet(el) => el.inner.ns,
            ElementNode::Template(el) => el.inner.ns,
        }
    }
    pub fn data(&self) -> (&String, bool, &Vec<TemplateChildNode>, &SourceLocation) {
        match &self {
            ElementNode::Plain(el) => (
                &el.inner.tag,
                el.inner.is_self_closing,
                &el.inner.children,
                &el.loc,
            ),
            ElementNode::Component(el) => (
                &el.inner.tag,
                el.inner.is_self_closing,
                &el.inner.children,
                &el.loc,
            ),
            ElementNode::SlotOutlet(el) => (
                &el.inner.tag,
                el.inner.is_self_closing,
                &el.inner.children,
                &el.loc,
            ),
            ElementNode::Template(el) => (
                &el.inner.tag,
                el.inner.is_self_closing,
                &el.inner.children,
                &el.loc,
            ),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum PlainElementCodegen {
    VNodeCall,
    /// when hoisted
    SimpleExpr,
    /// when cached by v-once
    CacheExpr,
    /// when cached by v-memo
    MemoExpr,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ComponentCodegen {
    VNodeCall,
    /// when cached by v-once
    CacheExpr,
    /// when cached by v-memo
    MemoExpr,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum SlotOutletCodegen {
    RenderSlotCall,
    /// when cached by v-once
    CacheExpr,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TemplateLit {
    elements: Vec<Node<JSChild>>,
}
