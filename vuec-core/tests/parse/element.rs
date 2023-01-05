use vuec_core::{
    ast::{
        attr::AttrsNode,
        el::ElementNode,
        template_child::{TemplateChildNode, Text},
        utils::{Position, SourceLocation},
        ConstantType, ElementType, NameSpace, Node,
    },
    errors::ErrorCode,
    options::{ErrorHandlingOptions, ParserOptions},
    parse::Parser,
    runtime_helpers::RuntimeHelper,
};

#[test]
fn simple_div() {
    let ast = Parser::base_parse("<div>hello</div>", None);
    let element = ast.inner.children.first().unwrap();

    assert_eq!(
        element,
        &TemplateChildNode::new_el(ElementNode::new(
            ElementType::Element,
            NameSpace::Html,
            "div".to_string(),
            false,
            vec![],
            vec![TemplateChildNode::new_text(Node::<Text>::new(
                "hello".to_string(),
                SourceLocation {
                    start: Position {
                        offset: 5,
                        line: 1,
                        column: 6,
                    },
                    end: Position {
                        offset: 10,
                        line: 1,
                        column: 11,
                    },
                    source: "hello".to_string(),
                },
            ))],
            SourceLocation {
                start: Position {
                    offset: 0,
                    line: 1,
                    column: 1,
                },
                end: Position {
                    offset: 16,
                    line: 1,
                    column: 17,
                },
                source: "<div>hello</div>".to_string(),
            },
        ))
    )
}

#[test]
fn empty() {
    let ast = Parser::base_parse("<div></div>", None);
    let element = ast.inner.children.first().unwrap();

    assert_eq!(
        element,
        &TemplateChildNode::new_el(ElementNode::new(
            ElementType::Element,
            NameSpace::Html,
            "div".to_string(),
            false,
            vec![],
            vec![],
            SourceLocation {
                start: Position {
                    offset: 0,
                    line: 1,
                    column: 1,
                },
                end: Position {
                    offset: 11,
                    line: 1,
                    column: 12,
                },
                source: "<div></div>".to_string(),
            },
        ))
    )
}

#[test]
fn self_closing() {
    let ast = Parser::base_parse("<div/>after", None);
    let element = ast.inner.children.first().unwrap();

    assert_eq!(
        element,
        &TemplateChildNode::new_el(ElementNode::new(
            ElementType::Element,
            NameSpace::Html,
            "div".to_string(),
            true,
            vec![],
            vec![],
            SourceLocation {
                start: Position {
                    offset: 0,
                    line: 1,
                    column: 1,
                },
                end: Position {
                    offset: 6,
                    line: 1,
                    column: 7,
                },
                source: "<div/>".to_string(),
            },
        ))
    )
}

#[test]
fn void_element() {
    let ast = Parser::base_parse(
        "<img>after",
        Some(ParserOptions {
            is_void_tag: |tag| tag == "img",
            ..Default::default()
        }),
    );

    let element = ast.inner.children.first().unwrap();

    assert_eq!(
        element,
        &TemplateChildNode::new_el(ElementNode::new(
            ElementType::Element,
            NameSpace::Html,
            "img".to_string(),
            false,
            vec![],
            vec![],
            SourceLocation {
                start: Position {
                    offset: 0,
                    line: 1,
                    column: 1,
                },
                end: Position {
                    offset: 5,
                    line: 1,
                    column: 6,
                },
                source: "<img>".to_string(),
            },
        ))
    )
}

#[test]
fn template_element_with_directives() {
    let ast = Parser::base_parse(r#"<template v-if="ok"></template>"#, None);
    let element = ast.inner.children.first().unwrap();

    assert!(matches!(
        element,
        TemplateChildNode::Element(ElementNode::Template(_))
    ));
}

#[test]
fn template_element_without_directives() {
    let ast = Parser::base_parse("<template></template>", None);
    let element = ast.inner.children.first().unwrap();

    assert!(matches!(
        element,
        TemplateChildNode::Element(ElementNode::Plain(_))
    ));
}

#[test]
fn native_element_with_is_native_tag() {
    let ast = Parser::base_parse(
        "<div></div><comp></comp><Comp></Comp>",
        Some(ParserOptions {
            is_native_tag: Some(|tag| tag == "div"),
            ..Default::default()
        }),
    );
    let children = ast.inner.children;

    assert!(matches!(
        children.get(0).unwrap(),
        TemplateChildNode::Element(ElementNode::Plain(_))
    ));
    assert!(matches!(
        children.get(1).unwrap(),
        TemplateChildNode::Element(ElementNode::Component(_))
    ));
    assert!(matches!(
        children.get(2).unwrap(),
        TemplateChildNode::Element(ElementNode::Component(_))
    ));
}

#[test]
fn native_element_without_is_native_tag() {
    let ast = Parser::base_parse("<div></div><comp></comp><Comp></Comp>", None);
    let children = ast.inner.children;

    assert!(matches!(
        children.get(0).unwrap(),
        TemplateChildNode::Element(ElementNode::Plain(_))
    ));
    assert!(matches!(
        children.get(1).unwrap(),
        TemplateChildNode::Element(ElementNode::Plain(_))
    ));
    assert!(matches!(
        children.get(2).unwrap(),
        TemplateChildNode::Element(ElementNode::Component(_))
    ));
}

#[test]
fn v_is_with_is_native_tag() {
    let ast = Parser::base_parse(
        r#"<div></div><div v-is="'foo'"></div><Comp></Comp>"#,
        Some(ParserOptions {
            is_native_tag: Some(|tag| tag == "div"),
            ..Default::default()
        }),
    );
    let children = ast.inner.children;

    assert!(matches!(
        children.get(0).unwrap(),
        TemplateChildNode::Element(ElementNode::Plain(_))
    ));
    assert!(matches!(
        children.get(1).unwrap(),
        TemplateChildNode::Element(ElementNode::Component(_))
    ));
    assert!(matches!(
        children.get(2).unwrap(),
        TemplateChildNode::Element(ElementNode::Component(_))
    ));
}

#[test]
fn v_is_without_is_native_tag() {
    let ast = Parser::base_parse(r#"<div></div><div v-is="'foo'"></div><Comp></Comp>"#, None);
    let children = ast.inner.children;

    assert!(matches!(
        children.get(0).unwrap(),
        TemplateChildNode::Element(ElementNode::Plain(_))
    ));
    assert!(matches!(
        children.get(1).unwrap(),
        TemplateChildNode::Element(ElementNode::Component(_))
    ));
    assert!(matches!(
        children.get(2).unwrap(),
        TemplateChildNode::Element(ElementNode::Component(_))
    ));
}

#[test]
fn custom_element() {
    let ast = Parser::base_parse(
        "<div></div><comp></comp>",
        Some(ParserOptions {
            is_native_tag: Some(|tag| tag == "div"),
            is_custom_element: |tag| tag == "comp",
            ..Default::default()
        }),
    );
    let children = ast.inner.children;

    assert!(matches!(
        children.get(0).unwrap(),
        TemplateChildNode::Element(ElementNode::Plain(_))
    ));
    assert!(matches!(
        children.get(1).unwrap(),
        TemplateChildNode::Element(ElementNode::Plain(_))
    ));
}

#[test]
fn builtin_component() {
    let ast = Parser::base_parse(
        "<div></div><comp></comp>",
        Some(ParserOptions {
            is_builtin_component: Some(|tag| {
                if tag == "comp" {
                    Some(RuntimeHelper::Custom)
                } else {
                    None
                }
            }),
            ..Default::default()
        }),
    );
    let children = ast.inner.children;

    assert!(matches!(
        children.get(0).unwrap(),
        TemplateChildNode::Element(ElementNode::Plain(_))
    ));
    assert!(matches!(
        children.get(1).unwrap(),
        TemplateChildNode::Element(ElementNode::Component(_))
    ));
}

#[test]
fn slot_element() {
    let ast = Parser::base_parse("<slot></slot><Comp></Comp>", None);
    let children = ast.inner.children;
    assert!(matches!(
        children.get(0).unwrap(),
        TemplateChildNode::Element(ElementNode::SlotOutlet(_))
    ));
    assert!(matches!(
        children.get(1).unwrap(),
        TemplateChildNode::Element(ElementNode::Component(_))
    ));
}

#[test]
fn attribute_with_no_value() {
    let ast = Parser::base_parse("<div id></div>", None);
    let element = ast.inner.children.first().unwrap();
    assert_eq!(
        element,
        &TemplateChildNode::Element(ElementNode::new(
            ElementType::Element,
            NameSpace::Html,
            "div".to_string(),
            false,
            vec![AttrsNode::new_attr(
                "id".to_string(),
                None,
                SourceLocation {
                    start: Position {
                        offset: 5,
                        line: 1,
                        column: 6,
                    },
                    end: Position {
                        offset: 7,
                        line: 1,
                        column: 8,
                    },
                    source: "id".to_string(),
                },
            )],
            vec![],
            SourceLocation {
                start: Position {
                    offset: 0,
                    line: 1,
                    column: 1,
                },
                end: Position {
                    offset: 14,
                    line: 1,
                    column: 15,
                },
                source: r#"<div id></div>"#.to_string(),
            },
        ))
    );
}

#[test]
/// attribute with empty value, double quote
fn attribute_with_empty_value_double_quote() {
    let ast = Parser::base_parse(r#"<div id=""></div>"#, None);
    let element = ast.inner.children.first().unwrap();
    assert_eq!(
        element,
        &TemplateChildNode::Element(ElementNode::new(
            ElementType::Element,
            NameSpace::Html,
            "div".to_string(),
            false,
            vec![AttrsNode::new_attr(
                "id".to_string(),
                Some(Node::<Text>::new(
                    "".to_string(),
                    SourceLocation {
                        start: Position {
                            offset: 8,
                            line: 1,
                            column: 9,
                        },
                        end: Position {
                            offset: 10,
                            line: 1,
                            column: 11,
                        },
                        source: r#""""#.to_string(),
                    },
                )),
                SourceLocation {
                    start: Position {
                        offset: 5,
                        line: 1,
                        column: 6,
                    },
                    end: Position {
                        offset: 10,
                        line: 1,
                        column: 11,
                    },
                    source: r#"id="""#.to_string(),
                },
            )],
            vec![],
            SourceLocation {
                start: Position {
                    offset: 0,
                    line: 1,
                    column: 1,
                },
                end: Position {
                    offset: 17,
                    line: 1,
                    column: 18,
                },
                source: r#"<div id=""></div>"#.to_string(),
            },
        ))
    );
}

#[test]
/// attribute with empty value, single quote
fn attribute_with_empty_value_single_quote() {
    let ast = Parser::base_parse(r#"<div id=''></div>"#, None);
    let element = ast.inner.children.first().unwrap();
    assert_eq!(
        element,
        &TemplateChildNode::Element(ElementNode::new(
            ElementType::Element,
            NameSpace::Html,
            "div".to_string(),
            false,
            vec![AttrsNode::new_attr(
                "id".to_string(),
                Some(Node::<Text>::new(
                    "".to_string(),
                    SourceLocation {
                        start: Position {
                            offset: 8,
                            line: 1,
                            column: 9,
                        },
                        end: Position {
                            offset: 10,
                            line: 1,
                            column: 11,
                        },
                        source: r#"''"#.to_string(),
                    },
                )),
                SourceLocation {
                    start: Position {
                        offset: 5,
                        line: 1,
                        column: 6,
                    },
                    end: Position {
                        offset: 10,
                        line: 1,
                        column: 11,
                    },
                    source: r#"id=''"#.to_string(),
                },
            )],
            vec![],
            SourceLocation {
                start: Position {
                    offset: 0,
                    line: 1,
                    column: 1,
                },
                end: Position {
                    offset: 17,
                    line: 1,
                    column: 18,
                },
                source: r#"<div id=''></div>"#.to_string(),
            },
        ))
    );
}

#[test]
/// attribute with value, double quote
fn attr_with_value_double_quote() {
    let ast = Parser::base_parse(r#"<div id=">'"></div>"#, None);
    let element = ast.inner.children.first().unwrap();
    assert_eq!(
        element,
        &TemplateChildNode::Element(ElementNode::new(
            ElementType::Element,
            NameSpace::Html,
            "div".to_string(),
            false,
            vec![AttrsNode::new_attr(
                "id".to_string(),
                Some(Node::<Text>::new(
                    ">'".to_string(),
                    SourceLocation {
                        start: Position {
                            offset: 8,
                            line: 1,
                            column: 9,
                        },
                        end: Position {
                            offset: 12,
                            line: 1,
                            column: 13,
                        },
                        source: r#"">'""#.to_string(),
                    },
                )),
                SourceLocation {
                    start: Position {
                        offset: 5,
                        line: 1,
                        column: 6,
                    },
                    end: Position {
                        offset: 12,
                        line: 1,
                        column: 13,
                    },
                    source: r#"id=">'""#.to_string(),
                },
            )],
            vec![],
            SourceLocation {
                start: Position {
                    offset: 0,
                    line: 1,
                    column: 1,
                },
                end: Position {
                    offset: 19,
                    line: 1,
                    column: 20,
                },
                source: r#"<div id=">'"></div>"#.to_string(),
            },
        ))
    );
}

#[test]
fn attr_with_value_single_quote() {
    let ast = Parser::base_parse(r#"<div id='>"'></div>"#, None);
    let element = ast.inner.children.first().unwrap();
    assert_eq!(
        element,
        &TemplateChildNode::Element(ElementNode::new(
            ElementType::Element,
            NameSpace::Html,
            "div".to_string(),
            false,
            vec![AttrsNode::new_attr(
                "id".to_string(),
                Some(Node::<Text>::new(
                    r#">""#.to_string(),
                    SourceLocation {
                        start: Position {
                            offset: 8,
                            line: 1,
                            column: 9,
                        },
                        end: Position {
                            offset: 12,
                            line: 1,
                            column: 13,
                        },
                        source: r#"'>"'"#.to_string(),
                    },
                )),
                SourceLocation {
                    start: Position {
                        offset: 5,
                        line: 1,
                        column: 6,
                    },
                    end: Position {
                        offset: 12,
                        line: 1,
                        column: 13,
                    },
                    source: r#"id='>"'"#.to_string(),
                },
            )],
            vec![],
            SourceLocation {
                start: Position {
                    offset: 0,
                    line: 1,
                    column: 1,
                },
                end: Position {
                    offset: 19,
                    line: 1,
                    column: 20,
                },
                source: r#"<div id='>"'></div>"#.to_string(),
            },
        ))
    );
}

#[test]
fn attr_with_value_unquoted() {
    let ast = Parser::base_parse(r#"<div id=a/></div>"#, None);
    let element = ast.inner.children.first().unwrap();
    assert_eq!(
        element,
        &TemplateChildNode::Element(ElementNode::new(
            ElementType::Element,
            NameSpace::Html,
            "div".to_string(),
            false,
            vec![AttrsNode::new_attr(
                "id".to_string(),
                Some(Node::<Text>::new(
                    "a/".to_string(),
                    SourceLocation {
                        start: Position {
                            offset: 8,
                            line: 1,
                            column: 9,
                        },
                        end: Position {
                            offset: 10,
                            line: 1,
                            column: 11,
                        },
                        source: r#"a/"#.to_string(),
                    },
                )),
                SourceLocation {
                    start: Position {
                        offset: 5,
                        line: 1,
                        column: 6,
                    },
                    end: Position {
                        offset: 10,
                        line: 1,
                        column: 11,
                    },
                    source: r#"id=a/"#.to_string(),
                },
            )],
            vec![],
            SourceLocation {
                start: Position {
                    offset: 0,
                    line: 1,
                    column: 1,
                },
                end: Position {
                    offset: 17,
                    line: 1,
                    column: 18,
                },
                source: r#"<div id=a/></div>"#.to_string(),
            },
        ))
    );
}

#[test]
fn multiple_attributes() {
    let ast = Parser::base_parse(r#"<div id=a class="c" inert style=''></div>"#, None);
    let element = ast.inner.children.first().unwrap();
    assert_eq!(
        element,
        &TemplateChildNode::Element(ElementNode::new(
            ElementType::Element,
            NameSpace::Html,
            "div".to_string(),
            false,
            vec![
                AttrsNode::new_attr(
                    "id".to_string(),
                    Some(Node::<Text>::new(
                        "a".to_string(),
                        SourceLocation {
                            start: Position {
                                offset: 8,
                                line: 1,
                                column: 9,
                            },
                            end: Position {
                                offset: 9,
                                line: 1,
                                column: 10,
                            },
                            source: r#"a"#.to_string(),
                        },
                    )),
                    SourceLocation {
                        start: Position {
                            offset: 5,
                            line: 1,
                            column: 6,
                        },
                        end: Position {
                            offset: 9,
                            line: 1,
                            column: 10,
                        },
                        source: r#"id=a"#.to_string(),
                    },
                ),
                AttrsNode::new_attr(
                    "class".to_string(),
                    Some(Node::<Text>::new(
                        "c".to_string(),
                        SourceLocation {
                            start: Position {
                                offset: 16,
                                line: 1,
                                column: 17,
                            },
                            end: Position {
                                offset: 19,
                                line: 1,
                                column: 20,
                            },
                            source: r#""c""#.to_string(),
                        },
                    )),
                    SourceLocation {
                        start: Position {
                            offset: 10,
                            line: 1,
                            column: 11,
                        },
                        end: Position {
                            offset: 19,
                            line: 1,
                            column: 20,
                        },
                        source: r#"class="c""#.to_string(),
                    },
                ),
                AttrsNode::new_attr(
                    "inert".to_string(),
                    None,
                    SourceLocation {
                        start: Position {
                            offset: 20,
                            line: 1,
                            column: 21,
                        },
                        end: Position {
                            offset: 25,
                            line: 1,
                            column: 26,
                        },
                        source: r#"inert"#.to_string(),
                    },
                ),
                AttrsNode::new_attr(
                    "style".to_string(),
                    Some(Node::<Text>::new(
                        "".to_string(),
                        SourceLocation {
                            start: Position {
                                offset: 32,
                                line: 1,
                                column: 33,
                            },
                            end: Position {
                                offset: 34,
                                line: 1,
                                column: 35,
                            },
                            source: "''".to_string(),
                        },
                    )),
                    SourceLocation {
                        start: Position {
                            offset: 26,
                            line: 1,
                            column: 27,
                        },
                        end: Position {
                            offset: 34,
                            line: 1,
                            column: 35,
                        },
                        source: "style=''".to_string(),
                    },
                ),
            ],
            vec![],
            SourceLocation {
                start: Position {
                    offset: 0,
                    line: 1,
                    column: 1,
                },
                end: Position {
                    offset: 41,
                    line: 1,
                    column: 42,
                },
                source: r#"<div id=a class="c" inert style=''></div>"#.to_string(),
            },
        ))
    );
}

#[test]
/// https://github.com/vuejs/core/issues/4251
fn class_attrs_should_ignore_whitespace_when_parsed() {
    let ast = Parser::base_parse("<div class=\" \n\t c \t\n \"></div>", None);
    let element = ast.inner.children.first().unwrap();
    assert_eq!(
        element,
        &TemplateChildNode::Element(ElementNode::new(
            ElementType::Element,
            NameSpace::Html,
            "div".to_string(),
            false,
            vec![AttrsNode::new_attr(
                "class".to_string(),
                Some(Node::<Text>::new(
                    "c".to_string(),
                    SourceLocation {
                        start: Position {
                            offset: 11,
                            line: 1,
                            column: 12,
                        },
                        end: Position {
                            offset: 22,
                            line: 3,
                            column: 3,
                        },
                        source: "\" \n\t c \t\n \"".to_string(),
                    },
                )),
                SourceLocation {
                    start: Position {
                        offset: 5,
                        line: 1,
                        column: 6,
                    },
                    end: Position {
                        offset: 22,
                        line: 3,
                        column: 3,
                    },
                    source: "class=\" \n\t c \t\n \"".to_string(),
                },
            )],
            vec![],
            SourceLocation {
                start: Position {
                    offset: 0,
                    line: 1,
                    column: 1,
                },
                end: Position {
                    offset: 29,
                    line: 3,
                    column: 10,
                },
                source: "<div class=\" \n\t c \t\n \"></div>".to_string(),
            },
        ))
    );
}

#[test]
fn directive_with_no_value() {
    let ast = Parser::base_parse("<div v-if/>", None);

    let directive = if let Some(TemplateChildNode::Element(ElementNode::Plain(n))) =
        ast.inner.children.first()
    {
        n.inner.props.first().unwrap()
    } else {
        panic!()
    };

    assert_eq!(
        directive,
        &AttrsNode::new_dir(
            "if".to_string(),
            None,
            None,
            vec![],
            SourceLocation {
                start: Position {
                    offset: 5,
                    line: 1,
                    column: 6,
                },
                end: Position {
                    offset: 9,
                    line: 1,
                    column: 10,
                },
                source: "v-if".to_string(),
            },
        )
    );
}

#[test]
fn directive_with_value() {
    let ast = Parser::base_parse(r#"<div v-if="a"/>"#, None);
    let directive = if let Some(TemplateChildNode::Element(ElementNode::Plain(n))) =
        ast.inner.children.first()
    {
        n.inner.props.first().unwrap()
    } else {
        panic!()
    };

    assert_eq!(
        directive,
        &AttrsNode::new_dir(
            "if".to_string(),
            None,
            Some(Node::new_simple_expr(
                "a".to_string(),
                false,
                ConstantType::NotConstant,
                SourceLocation {
                    start: Position {
                        offset: 11,
                        line: 1,
                        column: 12,
                    },
                    end: Position {
                        offset: 12,
                        line: 1,
                        column: 13,
                    },
                    source: "a".to_string(),
                },
            )),
            vec![],
            SourceLocation {
                start: Position {
                    offset: 5,
                    line: 1,
                    column: 6,
                },
                end: Position {
                    offset: 13,
                    line: 1,
                    column: 14,
                },
                source: r#"v-if="a""#.to_string(),
            },
        )
    );
}

#[test]
fn directive_with_argument() {
    let ast = Parser::base_parse(r#"<div v-on:click/>"#, None);
    let directive = if let Some(TemplateChildNode::Element(ElementNode::Plain(n))) =
        ast.inner.children.first()
    {
        n.inner.props.first().unwrap()
    } else {
        panic!()
    };

    assert_eq!(
        directive,
        &AttrsNode::new_dir(
            "on".to_string(),
            Some(Node::new_simple_expr(
                "click".to_string(),
                true,
                ConstantType::CanStringify,
                SourceLocation {
                    start: Position {
                        offset: 10,
                        line: 1,
                        column: 11,
                    },
                    end: Position {
                        offset: 15,
                        line: 1,
                        column: 16,
                    },
                    source: "click".to_string(),
                },
            )),
            None,
            vec![],
            SourceLocation {
                start: Position {
                    offset: 5,
                    line: 1,
                    column: 6,
                },
                end: Position {
                    offset: 15,
                    line: 1,
                    column: 16,
                },
                source: "v-on:click".to_string(),
            },
        )
    );
}

#[test]
fn directive_with_dyn_argument() {
    let ast = Parser::base_parse(r#"<div v-on:[event]/>"#, None);
    let directive = if let Some(TemplateChildNode::Element(ElementNode::Plain(n))) =
        ast.inner.children.first()
    {
        n.inner.props.first().unwrap()
    } else {
        panic!()
    };

    assert_eq!(
        directive,
        &AttrsNode::new_dir(
            "on".to_string(),
            Some(Node::new_simple_expr(
                "event".to_string(),
                false,
                ConstantType::NotConstant,
                SourceLocation {
                    start: Position {
                        offset: 10,
                        line: 1,
                        column: 11,
                    },
                    end: Position {
                        offset: 17,
                        line: 1,
                        column: 18,
                    },
                    source: "[event]".to_string(),
                },
            )),
            None,
            vec![],
            SourceLocation {
                start: Position {
                    offset: 5,
                    line: 1,
                    column: 6,
                },
                end: Position {
                    offset: 17,
                    line: 1,
                    column: 18,
                },
                source: "v-on:[event]".to_string(),
            },
        )
    );
}

#[test]
fn directive_with_a_modifier() {
    let ast = Parser::base_parse(r#"<div v-on.enter/>"#, None);
    let directive = if let Some(TemplateChildNode::Element(ElementNode::Plain(n))) =
        ast.inner.children.first()
    {
        n.inner.props.first().unwrap()
    } else {
        panic!()
    };

    assert_eq!(
        directive,
        &AttrsNode::new_dir(
            "on".to_string(),
            None,
            None,
            vec!["enter".to_string()],
            SourceLocation {
                start: Position {
                    offset: 5,
                    line: 1,
                    column: 6,
                },
                end: Position {
                    offset: 15,
                    line: 1,
                    column: 16,
                },
                source: "v-on.enter".to_string(),
            },
        )
    );
}

#[test]
fn directive_with_two_modifiers() {
    let ast = Parser::base_parse(r#"<div v-on.enter.exact/>"#, None);
    let directive = if let Some(TemplateChildNode::Element(ElementNode::Plain(n))) =
        ast.inner.children.first()
    {
        n.inner.props.first().unwrap()
    } else {
        panic!()
    };

    assert_eq!(
        directive,
        &AttrsNode::new_dir(
            "on".to_string(),
            None,
            None,
            vec!["enter".to_string(), "exact".to_string()],
            SourceLocation {
                start: Position {
                    offset: 5,
                    line: 1,
                    column: 6,
                },
                end: Position {
                    offset: 21,
                    line: 1,
                    column: 22,
                },
                source: "v-on.enter.exact".to_string(),
            },
        )
    );
}

#[test]
fn directive_with_argument_and_modifiers() {
    let ast = Parser::base_parse(r#"<div v-on:click.enter.exact/>"#, None);
    let directive = if let Some(TemplateChildNode::Element(ElementNode::Plain(n))) =
        ast.inner.children.first()
    {
        n.inner.props.first().unwrap()
    } else {
        panic!()
    };
    assert_eq!(
        directive,
        &AttrsNode::new_dir(
            "on".to_string(),
            Some(Node::new_simple_expr(
                "click".to_string(),
                true,
                ConstantType::CanStringify,
                SourceLocation {
                    start: Position {
                        offset: 10,
                        line: 1,
                        column: 11,
                    },
                    end: Position {
                        offset: 15,
                        line: 1,
                        column: 16,
                    },
                    source: "click".to_string(),
                },
            )),
            None,
            vec!["enter".to_string(), "exact".to_string()],
            SourceLocation {
                start: Position {
                    offset: 5,
                    line: 1,
                    column: 6,
                },
                end: Position {
                    offset: 27,
                    line: 1,
                    column: 28,
                },
                source: "v-on:click.enter.exact".to_string(),
            },
        )
    );
}

#[test]
fn directive_with_dyn_argument_and_modifiers() {
    let ast = Parser::base_parse(r#"<div v-on:[a.b].camel/>"#, None);
    let directive = if let Some(TemplateChildNode::Element(ElementNode::Plain(n))) =
        ast.inner.children.first()
    {
        n.inner.props.first().unwrap()
    } else {
        panic!()
    };
    assert_eq!(
        directive,
        &AttrsNode::new_dir(
            "on".to_string(),
            Some(Node::new_simple_expr(
                "a.b".to_string(),
                false,
                ConstantType::NotConstant,
                SourceLocation {
                    start: Position {
                        offset: 10,
                        line: 1,
                        column: 11,
                    },
                    end: Position {
                        offset: 15,
                        line: 1,
                        column: 16,
                    },
                    source: "[a.b]".to_string(),
                },
            )),
            None,
            vec!["camel".to_string()],
            SourceLocation {
                start: Position {
                    offset: 5,
                    line: 1,
                    column: 6,
                },
                end: Position {
                    offset: 21,
                    line: 1,
                    column: 22,
                },
                source: "v-on:[a.b].camel".to_string(),
            },
        )
    );
}

#[test]
fn directive_with_no_name() {
    let ast = Parser::base_parse(
        r#"<div v-/>"#,
        Some(ParserOptions {
            error_handling: ErrorHandlingOptions {
                on_error: |err| assert_eq!(err.code, ErrorCode::XMissingDirectiveName),
                ..Default::default()
            },
            ..Default::default()
        }),
    );

    let directive = if let Some(TemplateChildNode::Element(ElementNode::Plain(n))) =
        ast.inner.children.first()
    {
        n.inner.props.first().unwrap()
    } else {
        panic!()
    };

    assert_eq!(
        directive,
        &AttrsNode::new_attr(
            "v-".to_string(),
            None,
            SourceLocation {
                start: Position {
                    offset: 5,
                    line: 1,
                    column: 6,
                },
                end: Position {
                    offset: 7,
                    line: 1,
                    column: 8,
                },
                source: "v-".to_string(),
            },
        )
    );
}

#[test]
fn v_bind_shorthand() {
    let ast = Parser::base_parse(r#"<div :a=b />"#, None);
    let directive = if let Some(TemplateChildNode::Element(ElementNode::Plain(n))) =
        ast.inner.children.first()
    {
        n.inner.props.first().unwrap()
    } else {
        panic!()
    };

    assert_eq!(
        directive,
        &AttrsNode::new_dir(
            "bind".to_string(),
            Some(Node::new_simple_expr(
                "a".to_string(),
                true,
                ConstantType::CanStringify,
                SourceLocation {
                    start: Position {
                        offset: 6,
                        line: 1,
                        column: 7,
                    },
                    end: Position {
                        offset: 7,
                        line: 1,
                        column: 8,
                    },
                    source: "a".to_string(),
                },
            )),
            Some(Node::new_simple_expr(
                "b".to_string(),
                false,
                ConstantType::NotConstant,
                SourceLocation {
                    start: Position {
                        offset: 8,
                        line: 1,
                        column: 9,
                    },
                    end: Position {
                        offset: 9,
                        line: 1,
                        column: 10,
                    },
                    source: "b".to_string(),
                },
            )),
            vec![],
            SourceLocation {
                start: Position {
                    offset: 5,
                    line: 1,
                    column: 6,
                },
                end: Position {
                    offset: 9,
                    line: 1,
                    column: 10,
                },
                source: ":a=b".to_string(),
            },
        )
    );
}

#[test]
fn v_bind_dot_prop_shorthand() {
    let ast = Parser::base_parse(r#"<div .a=b />"#, None);
    let directive = if let Some(TemplateChildNode::Element(ElementNode::Plain(n))) =
        ast.inner.children.first()
    {
        n.inner.props.first().unwrap()
    } else {
        panic!()
    };
    assert_eq!(
        directive,
        &AttrsNode::new_dir(
            "bind".to_string(),
            Some(Node::new_simple_expr(
                "a".to_string(),
                true,
                ConstantType::CanStringify,
                SourceLocation {
                    start: Position {
                        offset: 6,
                        line: 1,
                        column: 7,
                    },
                    end: Position {
                        offset: 7,
                        line: 1,
                        column: 8,
                    },
                    source: "a".to_string(),
                },
            )),
            Some(Node::new_simple_expr(
                "b".to_string(),
                false,
                ConstantType::NotConstant,
                SourceLocation {
                    start: Position {
                        offset: 8,
                        line: 1,
                        column: 9,
                    },
                    end: Position {
                        offset: 9,
                        line: 1,
                        column: 10,
                    },
                    source: "b".to_string(),
                },
            )),
            vec!["prop".to_string()],
            SourceLocation {
                start: Position {
                    offset: 5,
                    line: 1,
                    column: 6,
                },
                end: Position {
                    offset: 9,
                    line: 1,
                    column: 10,
                },
                source: ".a=b".to_string(),
            },
        )
    );
}

#[test]
fn v_bind_shorthand_with_modifier() {
    let ast = Parser::base_parse(r#"<div :a.sync=b />"#, None);
    let directive = if let Some(TemplateChildNode::Element(ElementNode::Plain(n))) =
        ast.inner.children.first()
    {
        n.inner.props.first().unwrap()
    } else {
        panic!()
    };

    assert_eq!(
        directive,
        &AttrsNode::new_dir(
            "bind".to_string(),
            Some(Node::new_simple_expr(
                "a".to_string(),
                true,
                ConstantType::CanStringify,
                SourceLocation {
                    start: Position {
                        offset: 6,
                        line: 1,
                        column: 7,
                    },
                    end: Position {
                        offset: 7,
                        line: 1,
                        column: 8,
                    },
                    source: "a".to_string(),
                },
            )),
            Some(Node::new_simple_expr(
                "b".to_string(),
                false,
                ConstantType::NotConstant,
                SourceLocation {
                    start: Position {
                        offset: 13,
                        line: 1,
                        column: 14,
                    },
                    end: Position {
                        offset: 14,
                        line: 1,
                        column: 15,
                    },
                    source: "b".to_string(),
                },
            )),
            vec!["sync".to_string()],
            SourceLocation {
                start: Position {
                    offset: 5,
                    line: 1,
                    column: 6,
                },
                end: Position {
                    offset: 14,
                    line: 1,
                    column: 15,
                },
                source: ":a.sync=b".to_string(),
            },
        )
    );
}

#[test]
fn v_on_shorthand() {
    let ast = Parser::base_parse(r#"<div @a=b />"#, None);
    let directive = if let Some(TemplateChildNode::Element(ElementNode::Plain(n))) =
        ast.inner.children.first()
    {
        n.inner.props.first().unwrap()
    } else {
        panic!()
    };

    assert_eq!(
        directive,
        &AttrsNode::new_dir(
            "on".to_string(),
            Some(Node::new_simple_expr(
                "a".to_string(),
                true,
                ConstantType::CanStringify,
                SourceLocation {
                    start: Position {
                        offset: 6,
                        line: 1,
                        column: 7,
                    },
                    end: Position {
                        offset: 7,
                        line: 1,
                        column: 8,
                    },
                    source: "a".to_string(),
                },
            )),
            Some(Node::new_simple_expr(
                "b".to_string(),
                false,
                ConstantType::NotConstant,
                SourceLocation {
                    start: Position {
                        offset: 8,
                        line: 1,
                        column: 9,
                    },
                    end: Position {
                        offset: 9,
                        line: 1,
                        column: 10,
                    },
                    source: "b".to_string(),
                },
            )),
            vec![],
            SourceLocation {
                start: Position {
                    offset: 5,
                    line: 1,
                    column: 6,
                },
                end: Position {
                    offset: 9,
                    line: 1,
                    column: 10,
                },
                source: "@a=b".to_string(),
            },
        )
    );
}

#[test]
fn v_on_shorthand_with_modifier() {
    let ast = Parser::base_parse(r#"<div @a.enter=b />"#, None);
    let directive = if let Some(TemplateChildNode::Element(ElementNode::Plain(n))) =
        ast.inner.children.first()
    {
        n.inner.props.first().unwrap()
    } else {
        panic!()
    };
    assert_eq!(
        directive,
        &AttrsNode::new_dir(
            "on".to_string(),
            Some(Node::new_simple_expr(
                "a".to_string(),
                true,
                ConstantType::CanStringify,
                SourceLocation {
                    start: Position {
                        offset: 6,
                        line: 1,
                        column: 7,
                    },
                    end: Position {
                        offset: 7,
                        line: 1,
                        column: 8,
                    },
                    source: "a".to_string(),
                },
            )),
            Some(Node::new_simple_expr(
                "b".to_string(),
                false,
                ConstantType::NotConstant,
                SourceLocation {
                    start: Position {
                        offset: 14,
                        line: 1,
                        column: 15,
                    },
                    end: Position {
                        offset: 15,
                        line: 1,
                        column: 16,
                    },
                    source: "b".to_string(),
                },
            )),
            vec!["enter".to_string()],
            SourceLocation {
                start: Position {
                    offset: 5,
                    line: 1,
                    column: 6,
                },
                end: Position {
                    offset: 15,
                    line: 1,
                    column: 16,
                },
                source: "@a.enter=b".to_string(),
            },
        )
    );
}

#[test]
fn v_slot_shorthand() {
    let ast = Parser::base_parse(r#"<Comp #a="{ b }" />"#, None);
    let directive = if let Some(TemplateChildNode::Element(ElementNode::Component(n))) =
        ast.inner.children.first()
    {
        n.inner.props.first().unwrap()
    } else {
        panic!()
    };

    assert_eq!(
        directive,
        &AttrsNode::new_dir(
            "slot".to_string(),
            Some(Node::new_simple_expr(
                "a".to_string(),
                true,
                ConstantType::CanStringify,
                SourceLocation {
                    start: Position {
                        offset: 7,
                        line: 1,
                        column: 8,
                    },
                    end: Position {
                        offset: 8,
                        line: 1,
                        column: 9,
                    },
                    source: "a".to_string(),
                },
            )),
            Some(Node::new_simple_expr(
                "{ b }".to_string(),
                false,
                ConstantType::NotConstant,
                SourceLocation {
                    start: Position {
                        offset: 10,
                        line: 1,
                        column: 11,
                    },
                    end: Position {
                        offset: 15,
                        line: 1,
                        column: 16,
                    },
                    source: "{ b }".to_string(),
                },
            )),
            vec![],
            SourceLocation {
                start: Position {
                    offset: 6,
                    line: 1,
                    column: 7,
                },
                end: Position {
                    offset: 16,
                    line: 1,
                    column: 17,
                },
                source: r#"#a="{ b }""#.to_string(),
            },
        )
    );
}

#[test]
/// #1241 special case for 2.x compat
fn v_slot_arg_containing_dots() {}

#[test]
fn v_pre() {
    let ast = Parser::base_parse(
        "<div v-pre :id=\"foo\"><Comp/>{{ bar }}</div>\n<div :id=\"foo\"><Comp/>{{ bar }}</div>",
        None,
    );
    let div_with_pre = if let Some(TemplateChildNode::Element(ElementNode::Plain(n))) =
        ast.inner.children.first()
    {
        &n.inner
    } else {
        panic!()
    };
    assert_eq!(div_with_pre.props.len(), 1);
    assert_eq!(
        div_with_pre.props.first().unwrap(),
        &AttrsNode::new_attr(
            ":id".to_string(),
            Some(Node::<Text>::new(
                "foo".to_string(),
                SourceLocation {
                    start: Position {
                        offset: 15,
                        line: 1,
                        column: 16,
                    },
                    end: Position {
                        offset: 20,
                        line: 1,
                        column: 21,
                    },
                    source: r#""foo""#.to_string(),
                },
            )),
            SourceLocation {
                start: Position {
                    offset: 11,
                    line: 1,
                    column: 12,
                },
                end: Position {
                    offset: 20,
                    line: 1,
                    column: 21,
                },
                source: r#":id="foo""#.to_string(),
            },
        )
    );

    assert!(matches!(
        div_with_pre.children.get(0).unwrap(),
        TemplateChildNode::Element(ElementNode::Plain(_))
    ));
    assert!(matches!(
        div_with_pre.children.get(1).unwrap(),
        TemplateChildNode::Text(_)
    ));

    let div_without_pre = if let Some(TemplateChildNode::Element(ElementNode::Plain(n))) =
        ast.inner.children.get(1)
    {
        &n.inner
    } else {
        panic!()
    };
    assert_eq!(div_without_pre.props.len(), 1);
    assert_eq!(
        div_without_pre.props.first().unwrap(),
        &AttrsNode::new_dir(
            "bind".to_string(),
            Some(Node::new_simple_expr(
                "id".to_string(),
                true,
                ConstantType::CanStringify,
                SourceLocation {
                    start: Position {
                        offset: 50,
                        line: 2,
                        column: 7,
                    },
                    end: Position {
                        offset: 52,
                        line: 2,
                        column: 9,
                    },
                    source: "id".to_string(),
                },
            )),
            Some(Node::new_simple_expr(
                "foo".to_string(),
                false,
                ConstantType::NotConstant,
                SourceLocation {
                    start: Position {
                        offset: 54,
                        line: 2,
                        column: 11,
                    },
                    end: Position {
                        offset: 57,
                        line: 2,
                        column: 14,
                    },
                    source: "foo".to_string(),
                },
            )),
            vec![],
            SourceLocation {
                start: Position {
                    offset: 49,
                    line: 2,
                    column: 6,
                },
                end: Position {
                    offset: 58,
                    line: 2,
                    column: 15,
                },
                source: r#":id="foo""#.to_string(),
            },
        )
    );

    assert!(matches!(
        div_without_pre.children.get(0).unwrap(),
        TemplateChildNode::Element(ElementNode::Component(_))
    ));
    assert!(matches!(
        div_without_pre.children.get(1).unwrap(),
        TemplateChildNode::Interpolation(_)
    ));
}

#[test]
fn self_closing_v_pre() {
    let ast = Parser::base_parse(
        "<div v-pre/>\n<div :id=\"foo\"><Comp/>{{ bar }}</div>",
        None,
    );
    // should not affect siblings after it
    let div_without_pre = if let Some(TemplateChildNode::Element(ElementNode::Plain(n))) =
        ast.inner.children.get(1)
    {
        &n.inner
    } else {
        panic!()
    };

    assert_eq!(div_without_pre.props.len(), 1);
    assert_eq!(
        div_without_pre.props.first().unwrap(),
        &AttrsNode::new_dir(
            "bind".to_string(),
            Some(Node::new_simple_expr(
                "id".to_string(),
                true,
                ConstantType::CanStringify,
                SourceLocation {
                    start: Position {
                        offset: 19,
                        line: 2,
                        column: 7,
                    },
                    end: Position {
                        offset: 21,
                        line: 2,
                        column: 9,
                    },
                    source: "id".to_string(),
                },
            )),
            Some(Node::new_simple_expr(
                "foo".to_string(),
                false,
                ConstantType::NotConstant,
                SourceLocation {
                    start: Position {
                        offset: 23,
                        line: 2,
                        column: 11,
                    },
                    end: Position {
                        offset: 26,
                        line: 2,
                        column: 14,
                    },
                    source: "foo".to_string(),
                },
            )),
            vec![],
            SourceLocation {
                start: Position {
                    offset: 18,
                    line: 2,
                    column: 6,
                },
                end: Position {
                    offset: 27,
                    line: 2,
                    column: 15,
                },
                source: r#":id="foo""#.to_string(),
            },
        )
    );

    assert!(matches!(
        div_without_pre.children.get(0).unwrap(),
        TemplateChildNode::Element(ElementNode::Component(_))
    ));
    assert!(matches!(
        div_without_pre.children.get(1).unwrap(),
        TemplateChildNode::Interpolation(_)
    ));
}

#[test]
fn end_tags_case_insensitive() {
    let ast = Parser::base_parse("<div>hello</DIV>after", None);
    let text = if let Some(TemplateChildNode::Element(ElementNode::Plain(el))) =
        ast.inner.children.first()
    {
        el.inner.children.first().unwrap()
    } else {
        panic!()
    };

    assert_eq!(
        text,
        &TemplateChildNode::Text(Node::<Text>::new(
            "hello".to_string(),
            SourceLocation {
                start: Position {
                    offset: 5,
                    line: 1,
                    column: 6,
                },
                end: Position {
                    offset: 10,
                    line: 1,
                    column: 11,
                },
                source: "hello".to_string(),
            },
        ))
    );
}
