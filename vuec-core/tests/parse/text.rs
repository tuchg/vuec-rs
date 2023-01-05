use vuec_core::{
    ast::{
        template_child::{TemplateChildNode, Text},
        utils::{Position, SourceLocation},
        Node,
    },
    errors::{CompilerError, ErrorCode},
    options::{ErrorHandlingOptions, ParserOptions},
    parse::Parser,
};

#[test]
fn simple_text() {
    let ast = Parser::base_parse("some text", None);
    let text_node = ast.inner.children.first();

    assert_eq!(
        text_node,
        Some(&TemplateChildNode::Text(Node::<Text>::new(
            "some text".to_string(),
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
                source: "some text".to_string(),
            },
        )))
    );
}

#[test]
fn text_with_invalid_end_tag() {
    let ast = Parser::base_parse(
        "some text</div>",
        Some(ParserOptions {
            error_handling: ErrorHandlingOptions {
                on_error: |err: CompilerError| {
                    assert_eq!(err.code, ErrorCode::XInvalidEndTag);
                },
                ..Default::default()
            },
            ..Default::default()
        }),
    );
    let text_node = ast.inner.children.first();

    assert_eq!(
        text_node,
        Some(&TemplateChildNode::Text(Node::<Text>::new(
            "some text".to_string(),
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
                source: "some text".to_string(),
            },
        )))
    );
}

#[test]
fn text_with_interpolation() {
    let ast = Parser::base_parse("some {{ foo + bar }} text", None);
    let text1 = ast.inner.children.get(0);
    let text2 = ast.inner.children.get(2);

    assert_eq!(
        text1,
        Some(&TemplateChildNode::Text(Node::<Text>::new(
            "some ".to_string(),
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
                source: "some ".to_string(),
            },
        )))
    );

    assert_eq!(
        text2,
        Some(&TemplateChildNode::Text(Node::<Text>::new(
            " text".to_string(),
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
                source: " text".to_string(),
            },
        )))
    )
}

#[test]
fn text_with_interpolation_which_has_lt() {
    let ast = Parser::base_parse("some {{ a<b && c>d }} text", None);
    let text1 = ast.inner.children.get(0).unwrap();
    let text2 = ast.inner.children.get(2).unwrap();

    assert_eq!(
        text1,
        &TemplateChildNode::Text(Node::<Text>::new(
            "some ".to_string(),
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
                source: "some ".to_string(),
            },
        ))
    );
    assert_eq!(
        text2,
        &TemplateChildNode::Text(Node::<Text>::new(
            " text".to_string(),
            SourceLocation {
                start: Position {
                    offset: 21,
                    line: 1,
                    column: 22,
                },
                end: Position {
                    offset: 26,
                    line: 1,
                    column: 27,
                },
                source: " text".to_string(),
            },
        ))
    )
}

#[test]
fn text_with_mix_of_tags_and_interpolations() {
    let ast = Parser::base_parse("some <span>{{ foo < bar + foo }} text</span>", None);
    let text1 = ast.inner.children.get(0).unwrap();
    let text2;
    if let TemplateChildNode::Element(el) = ast.inner.children.get(1).unwrap() {
        text2 = el.children().get(1).unwrap();
    } else {
        panic!("not element");
    }

    assert_eq!(
        text1,
        &TemplateChildNode::Text(Node::<Text>::new(
            "some ".to_string(),
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
                source: "some ".to_string(),
            },
        ))
    );

    assert_eq!(
        text2,
        &TemplateChildNode::Text(Node::<Text>::new(
            " text".to_string(),
            SourceLocation {
                start: Position {
                    offset: 32,
                    line: 1,
                    column: 33,
                },
                end: Position {
                    offset: 37,
                    line: 1,
                    column: 38,
                },
                source: " text".to_string(),
            },
        ))
    )
}

#[test]
/// lonely "<" doesn't separate nodes
fn text_with_lonely_lt_dont_separate_nodes() {
    let ast = Parser::base_parse(
        "a < b",
        Some(ParserOptions {
            error_handling: ErrorHandlingOptions {
                on_error: |err| {
                    assert_eq!(err.code, ErrorCode::InvalidFirstCharacterOfTagName);
                },
                ..Default::default()
            },
            ..Default::default()
        }),
    );

    let text = ast.inner.children.get(0).unwrap();

    assert_eq!(
        text,
        &TemplateChildNode::Text(Node::<Text>::new(
            "a < b".to_string(),
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
                source: "a < b".to_string(),
            },
        ))
    );
}

#[test]
/// lonely "{{" doesn't separate nodes
fn text_with_lonely_delimiters_dont_separate_nodes() {
    let ast = Parser::base_parse(
        "a {{ b",
        Some(ParserOptions {
            error_handling: ErrorHandlingOptions {
                on_error: |err| {
                    assert_eq!(err.code, ErrorCode::XMissingInterpolationEnd);
                },
                ..Default::default()
            },
            ..Default::default()
        }),
    );

    let text = ast.inner.children.get(0).unwrap();

    assert_eq!(
        text,
        &TemplateChildNode::Text(Node::<Text>::new(
            "a {{ b".to_string(),
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
                source: "a {{ b".to_string(),
            },
        ))
    );
}
