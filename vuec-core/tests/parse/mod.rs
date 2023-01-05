use vuec_core::{
    ast::{
        el::ElementNode,
        template_child::{
            TemplateChildNode,
            TemplateChildNode::{Interpolation, Text},
        },
        utils::Position,
    },
    errors::ErrorCode,
    options::{ErrorHandlingOptions, ParserOptions},
    parse::Parser,
};

mod comment;
mod decode_entities;
mod element;
mod errors;
mod interpolation;
mod text;
mod whitespace_condense;
mod whitespace_preserve;

#[test]
fn parse_with_correct_location_info() {
    if let [Text(foo), Interpolation(bar), Text(but), Interpolation(baz)] = &Parser::base_parse(
        r#"
foo
 is {{ bar }} but {{ baz }}"#
            .trim(),
        None,
    )
    .inner
    .children[..]
    {
        let mut offset = 0;
        assert_eq!(
            foo.loc.start,
            Position {
                offset,
                line: 1,
                column: 1,
            }
        );
        offset += foo.loc.source.len();
        assert_eq!(
            foo.loc.end,
            Position {
                offset,
                line: 2,
                column: 5,
            }
        );

        assert_eq!(
            bar.loc.start,
            Position {
                offset,
                line: 2,
                column: 5,
            }
        );
        let bar_inner = &bar.inner.content;
        offset += 3;
        assert_eq!(
            bar_inner.loc.start,
            Position {
                offset,
                line: 2,
                column: 8,
            }
        );
        offset += bar_inner.loc.source.len();
        assert_eq!(
            bar_inner.loc.end,
            Position {
                offset,
                line: 2,
                column: 11,
            }
        );
        offset += 3;
        assert_eq!(
            bar.loc.end,
            Position {
                offset,
                line: 2,
                column: 14,
            }
        );
        assert_eq!(
            but.loc.start,
            Position {
                offset,
                line: 2,
                column: 14,
            }
        );
        offset += but.loc.source.len();
        assert_eq!(
            but.loc.end,
            Position {
                offset,
                line: 2,
                column: 19,
            }
        );

        assert_eq!(
            baz.loc.start,
            Position {
                offset,
                line: 2,
                column: 19,
            }
        );
        let baz_inner = &baz.inner.content;
        offset += 3;
        assert_eq!(
            baz_inner.loc.start,
            Position {
                offset,
                line: 2,
                column: 22,
            }
        );
        offset += baz_inner.loc.source.len();
        assert_eq!(
            baz_inner.loc.end,
            Position {
                offset,
                line: 2,
                column: 25,
            }
        );
        offset += 3;
        assert_eq!(
            baz.loc.end,
            Position {
                offset,
                line: 2,
                column: 28,
            }
        );
    } else {
        panic!()
    }
}

#[test]
fn self_closing_single_tag() {
    let ast = Parser::base_parse(r#"<div :class="{ some: condition }" />"#, None);
    assert_eq!(ast.inner.children.len(), 1);
    assert!(matches!(
        ast.inner.children.first().unwrap(),
        TemplateChildNode::Element(ElementNode::Plain(_))
    ));
}

#[test]
fn self_closing_multiple_tag() {
    let ast = Parser::base_parse(
        r#"<div :class="{ some: condition }" />
<p v-bind:style="{ color: 'red' }"/>"#,
        None,
    );

    assert_eq!(ast.inner.children.len(), 2);
    assert!(matches!(
        ast.inner.children.first().unwrap(),
        TemplateChildNode::Element(ElementNode::Plain(_))
    ));
    assert!(matches!(
        ast.inner.children.last().unwrap(),
        TemplateChildNode::Element(ElementNode::Plain(_))
    ));
}

#[test]
fn valid_html() {
    let ast = Parser::base_parse(
        r#"<div :class="{ some: condition }">
  <p v-bind:style="{ color: 'red' }"/>
  <!-- a comment with <html> inside it -->
</div>"#,
        None,
    );
    assert_eq!(ast.inner.children.len(), 1);
    assert!(matches!(
        ast.inner.children.first().unwrap(),
        TemplateChildNode::Element(ElementNode::Plain(_))
    ));
    let el_children = if let Some(TemplateChildNode::Element(ElementNode::Plain(element))) =
        ast.inner.children.first()
    {
        assert_eq!(element.inner.tag, "div");
        &element.inner.children
    } else {
        panic!();
    };

    assert_eq!(el_children.len(), 2);
    assert!(matches!(
        el_children.first().unwrap(),
        TemplateChildNode::Element(ElementNode::Plain(_))
    ));
    assert!(matches!(
        el_children.last().unwrap(),
        TemplateChildNode::Comment(_)
    ));
}

#[test]
fn invalid_html() {
    let ast = Parser::base_parse(
        "<div>\n<span>\n</div>\n</span>",
        Some(ParserOptions {
            error_handling: ErrorHandlingOptions {
                on_error: |err| match err.code {
                    ErrorCode::XMissingEndTag => {
                        assert_eq!(
                            err.loc.unwrap().start,
                            Position {
                                offset: 6,
                                line: 2,
                                column: 1,
                            }
                        )
                    }
                    ErrorCode::XInvalidEndTag => {
                        assert_eq!(
                            err.loc.unwrap().start,
                            Position {
                                offset: 20,
                                line: 4,
                                column: 1,
                            }
                        )
                    }
                    _ => panic!("Unexpected error: {:?}", err),
                },
                ..Default::default()
            },
            ..Default::default()
        }),
    );
    assert_eq!(ast.inner.children.len(), 1);
}
