pub mod attr;
pub mod el;
pub mod expr;
pub mod js_child;
pub mod parent;
pub mod stmt;
pub mod template_child;
pub mod utils;

use std::fmt::Debug;

use crate::ast::utils::SourceLocation;

/// Vue template is a platform-agnostic superset of HTML (syntax only).
/// More namespaces like SVG and MathML are declared by platform specific
/// compilers.
#[derive(Clone, PartialEq, Debug, Copy, Eq)]
pub enum NameSpace {
    Html,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Node<N: Clone + Debug + PartialEq + Eq> {
    pub kind: NodeType,
    pub loc: SourceLocation,
    pub inner: N,
}

impl<N: Clone + Debug + PartialEq + Eq> Eq for Node<N> {}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum NodeType {
    Root,
    Element,
    Text,
    Comment,
    SimpleExpr,
    Interpolation,
    Attribute,
    Directive,
    CompoundExpr,
    If,
    IfBranch,
    For,
    TextCall,
    VNodeCall,
    JSCallExpr,
    JSObjectExpr,
    JSProperty,
    JSArrayExpr,
    JSFunctionExpr,
    JSConditionalExpr,
    JSCacheExpr,
    JSBlockStmt,
    JSTemplateLiteral,
    JSIfStmt,
    JSAssignmentExpr,
    JSSequenceExpr,
    JSReturnStmt,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ElementType {
    Element,
    Component,
    Slot,
    Template,
}

/// Static types have several levels.
/// Higher levels implies lower levels. e.g. a node that can be stringified
/// can always be hoisted and skipped for patch.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ConstantType {
    NotConstant,
    CanSkipPatch,
    CanHoist,
    CanStringify,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct PhantomType;
