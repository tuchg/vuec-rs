use std::{cell::RefCell, rc::Rc};

use vuec_core::{
    options::TransformOptions,
    parse::Parser,
    transform::{NodeTransform, Transform},
};

mod element;
mod expr;
mod hoist_static;
mod noop_directive;
mod root_codegen;
mod slot_outlet;
mod text;
mod v_bind;
mod v_for;
mod v_if;
mod v_memo;
mod v_model;
mod v_on;
mod v_once;
mod v_slot;

#[test]
fn context_state() {
    let ast = Parser::base_parse("<div>hello {{ world }}</div>", None);
    // manually store call arguments because context is mutable and shared
    // across calls
    // let mut calls = vec![];
    // let plugin: NodeTransform = |node| {
    //     calls.push((node.clone()));
    //     vec![]
    // };
    //
    // Transform::transform(Rc::new(RefCell::new(ast)), Some(TransformOptions {
    //     node_transforms: vec![plugin],
    //     ..Default::default()
    // }));
}

#[test]
fn context_replace_node() {}

#[test]
fn context_remove_node() {}

#[test]
fn context_remove_node_prev_sibling() {}

#[test]
fn context_remove_node_next_sibling() {}

#[test]
fn context_hoist() {}

#[test]
fn on_error_option() {}

#[test]
fn should_inject_tostring_helper_for_interpolations() {}

#[test]
fn should_inject_createvnode_and_comment_for_comments() {}
