use vuec_core::{
    ast::{
        expr::ExprNode,
        template_child::{Interpolation, TemplateChildNode},
        utils::{Position, SourceLocation},
        ConstantType, Node,
    },
    options::ParserOptions,
    parse::Parser,
};

#[test]
fn simple_interpolation() {
    let ast = Parser::base_parse("{{message}}", None);
    let interpolation = ast.inner.children.first().unwrap();

    assert_eq!(
        interpolation,
        &TemplateChildNode::new_interpolation(Node::<Interpolation>::new(
            ExprNode::new_simple_expr(
                "message".to_string(),
                false,
                ConstantType::NotConstant,
                SourceLocation {
                    start: Position {
                        offset: 2,
                        line: 1,
                        column: 3,
                    },
                    end: Position {
                        offset: 9,
                        line: 1,
                        column: 10,
                    },
                },
            ),
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
            },
        ))
    );
}

#[test]
fn it_can_have_tag_like_notation() {
    let ast = Parser::base_parse("{{ a<b }}", None);
    let interpolation = ast.inner.children.first().unwrap();

    assert_eq!(
        interpolation,
        &TemplateChildNode::new_interpolation(Node::<Interpolation>::new(
            ExprNode::new_simple_expr(
                "a<b".to_string(),
                false,
                ConstantType::NotConstant,
                SourceLocation {
                    start: Position {
                        offset: 3,
                        line: 1,
                        column: 4,
                    },
                    end: Position {
                        offset: 6,
                        line: 1,
                        column: 7,
                    },
                },
            ),
            SourceLocation {
                start: Position {
                    offset: 0,
                    line: 1,
                    column: 1,
                },
                end: Position {
                    offset: 9,
                    line: 1,
                    column: 10,
                },
            },
        ))
    );
}

#[test]
fn it_can_have_tag_like_notation_3() {
    let ast = Parser::base_parse(r#"<div>{{ "</div>" }}</div>"#, None);
    let element = ast.inner.children.first().unwrap();
    let interpolation = if let TemplateChildNode::Element(el) = element {
        el.children().first().unwrap()
    } else {
        panic!("not element");
    };
    assert_eq!(
        interpolation,
        &TemplateChildNode::new_interpolation(Node::<Interpolation>::new(
            ExprNode::new_simple_expr(
                r#""</div>""#.to_string(),
                false,
                // The `constType` is the default value and will be determined in
                // `transformExpression`.
                ConstantType::NotConstant,
                SourceLocation {
                    start: Position {
                        offset: 8,
                        line: 1,
                        column: 9,
                    },
                    end: Position {
                        offset: 16,
                        line: 1,
                        column: 17,
                    },
                },
            ),
            SourceLocation {
                start: Position {
                    offset: 5,
                    line: 1,
                    column: 6,
                },
                end: Position {
                    offset: 19,
                    line: 1,
                    column: 20,
                },
            },
        ))
    );
}

#[test]
fn custom_delimiters() {
    let ast = Parser::base_parse(
        "<p>{msg}</p>",
        Some(ParserOptions {
            delimiters: ("{", "}"),
            ..Default::default()
        }),
    );
    let element = ast.inner.children.first().unwrap();
    let interpolation = if let TemplateChildNode::Element(el) = element {
        el.children().first().unwrap()
    } else {
        panic!("not element");
    };
    assert_eq!(
        interpolation,
        &TemplateChildNode::new_interpolation(Node::<Interpolation>::new(
            ExprNode::new_simple_expr(
                "msg".to_string(),
                false,
                ConstantType::NotConstant,
                SourceLocation {
                    start: Position {
                        offset: 4,
                        line: 1,
                        column: 5,
                    },
                    end: Position {
                        offset: 7,
                        line: 1,
                        column: 8,
                    },
                },
            ),
            SourceLocation {
                start: Position {
                    offset: 3,
                    line: 1,
                    column: 4,
                },
                end: Position {
                    offset: 8,
                    line: 1,
                    column: 9,
                },
            },
        ))
    );
}
