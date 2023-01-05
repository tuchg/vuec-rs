use crate::ast::{
    expr::Expr,
    parent::Parent,
    template_child::{TemplateChildNode, Text},
    Node,
};

pub mod v_for;

/// NodeTransform:
///   Transforms that operate directly on a ChildNode. NodeTransforms may mutate,
///   replace or remove the node being processed.
pub type NodeTransform = fn(node: Node<Text>, context: TransformContext);

/// DirectiveTransform:
///   Transforms that handles a single directive attribute on an element.
///   It translates the raw directive into actual props for the VNode.
pub type DirectiveTransform = fn();

/// A structural directive transform is technically also a NodeTransform;
/// Only v-if and v-for fall into this category.
pub type StructureDirectiveTransform = fn();

pub type HoistTransform =
    fn(children: Vec<TemplateChildNode>, context: TransformContext, parent: Node<Parent>);

pub struct TransformContext;

#[derive(Clone, PartialEq, Debug, Eq)]
pub struct ImportItem {
    exp: Node<Expr>,
    path: String,
}
