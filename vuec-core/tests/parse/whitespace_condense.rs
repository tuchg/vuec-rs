// whitespace management when adopting strategy condense

use vuec_core::{
    ast::{parent::Root, template_child::TemplateChildNode, Node},
    options::{ParserOptions, WhiteSpaceStrategy},
    parse::{Parser, TextMode},
};

fn parse(content: &str, options: Option<ParserOptions>) -> Node<Root> {
    Parser::base_parse(
        content,
        Some(ParserOptions {
            whitespace: WhiteSpaceStrategy::Condense,
            ..options.unwrap_or_default()
        }),
    )
}

#[test]
/// should remove whitespaces at start/end inside an element
fn should_remove_whitespace_at_start_or_end_inside_an_element() {
    let ast = parse("<div>   <span/>    </div>", None);
    if let Some(TemplateChildNode::Element(el)) = ast.inner.children.first() {
        assert_eq!(el.children().len(), 1);
    } else {
        panic!()
    };
}

#[test]
/// should remove whitespaces w/ newline between elements
fn should_remove_whitespace_with_newline_between_elements() {
    let ast = parse("<div/> \n <div/> \n <div/>", None);

    assert_eq!(ast.inner.children.len(), 3);
    assert_eq!(
        ast.inner
            .children
            .iter()
            .find(|child| !matches!(child, TemplateChildNode::Element(_))),
        None
    )
}

#[test]
/// should remove whitespaces adjacent to comments
fn should_remove_whitespace_adjacent_to_comments() {
    let ast = parse("<div/> \n <!--foo--> <div/>", None);
    assert_eq!(ast.inner.children.len(), 3);
    assert!(matches!(
        ast.inner.children[0],
        TemplateChildNode::Element(_)
    ));
    assert!(matches!(
        ast.inner.children[1],
        TemplateChildNode::Comment(_)
    ));
    assert!(matches!(
        ast.inner.children[2],
        TemplateChildNode::Element(_)
    ));
}

#[test]
/// should remove whitespaces w/ newline between comments and elements
fn should_remove_whitespace_with_newline_between_comments_and_elements() {
    let ast = parse("<div/> \n <!--foo--> \n <div/>", None);
    assert_eq!(ast.inner.children.len(), 3);
    assert!(matches!(
        ast.inner.children[0],
        TemplateChildNode::Element(_)
    ));
    assert!(matches!(
        ast.inner.children[1],
        TemplateChildNode::Comment(_)
    ));
    assert!(matches!(
        ast.inner.children[2],
        TemplateChildNode::Element(_)
    ));
}

#[test]
/// should NOT remove whitespaces w/ newline between interpolations
fn should_not_remove_whitespace_with_newline_between_interpolations() {
    let ast = parse("{{ foo }} \n {{ bar }}", None);
    assert_eq!(ast.inner.children.len(), 3);
    assert!(matches!(
        ast.inner.children[0],
        TemplateChildNode::Interpolation(_)
    ));
    assert!(matches!(ast.inner.children[1], TemplateChildNode::Text(_)));
    assert!(matches!(
        ast.inner.children[2],
        TemplateChildNode::Interpolation(_)
    ));
}

#[test]
/// should NOT remove whitespaces w/ newline between interpolation and comment
fn should_not_remove_whitespace_with_newline_between_interpolation_and_comment() {
    let ast = parse("<!-- foo --> \n {{msg}}", None);
    assert_eq!(ast.inner.children.len(), 3);
    assert!(matches!(
        ast.inner.children[0],
        TemplateChildNode::Comment(_)
    ));
    assert!(matches!(ast.inner.children[1], TemplateChildNode::Text(_)));
    assert!(matches!(
        ast.inner.children[2],
        TemplateChildNode::Interpolation(_)
    ));
}

#[test]
/// should NOT remove whitespaces w/o newline between elements
fn should_not_remove_whitespace_without_newline_between_elements() {
    let ast = parse("<div/> <div/> <div/>", None);
    assert_eq!(ast.inner.children.len(), 5);
    assert!(matches!(
        ast.inner.children[0],
        TemplateChildNode::Element(_)
    ));
    assert!(matches!(ast.inner.children[1], TemplateChildNode::Text(_)));
    assert!(matches!(
        ast.inner.children[2],
        TemplateChildNode::Element(_)
    ));
    assert!(matches!(ast.inner.children[3], TemplateChildNode::Text(_)));
    assert!(matches!(
        ast.inner.children[4],
        TemplateChildNode::Element(_)
    ));
}

#[test]
/// should condense consecutive whitespaces in text
fn should_condense_consecutive_whitespaces_in_text() {
    let ast = parse("   foo  \n    bar     baz     ", None);

    if let Some(TemplateChildNode::Text(text)) = ast.inner.children.first() {
        assert_eq!(text.inner.content, " foo bar baz ");
    } else {
        panic!()
    };
}

#[test]
/// should remove leading newline character immediately following the pre element start tag
fn should_remove_leading_newline_character_immediately_following_the_pre_element_start_tag() {
    let ast = Parser::base_parse(
        "<pre>\n  foo  bar  </pre>",
        Some(ParserOptions {
            is_pre_tag: |tag| tag == "pre",
            ..Default::default()
        }),
    );
    assert_eq!(ast.inner.children.len(), 1);
    if let Some(TemplateChildNode::Element(pre_element)) = ast.inner.children.first() {
        assert_eq!(pre_element.children().len(), 1);
        if let Some(TemplateChildNode::Text(text)) = pre_element.children().first() {
            assert_eq!(text.inner.content, "  foo  bar  ");
        } else {
            panic!()
        }
    } else {
        panic!()
    };
}

#[test]
/// should NOT remove leading newline character immediately following child-tag of pre element
fn should_not_remove_leading_newline_character_immediately_following_child_tag_of_pre_element() {
    let ast = Parser::base_parse(
        "<pre><span></span>\n  foo  bar  </pre>",
        Some(ParserOptions {
            is_pre_tag: |tag| tag == "pre",
            ..Default::default()
        }),
    );
    if let Some(TemplateChildNode::Element(pre_element)) = ast.inner.children.first() {
        assert_eq!(pre_element.children().len(), 2);
        if let Some(TemplateChildNode::Text(node)) = pre_element.children().get(1) {
            assert_eq!(node.inner.content, "\n  foo  bar  ");
        } else {
            panic!()
        };
    } else {
        panic!()
    };
}

#[test]
/// self-closing pre tag
fn self_closing_pre_tag() {
    let ast = Parser::base_parse(
        "<pre/><span>\n  foo   bar</span>",
        Some(ParserOptions {
            is_pre_tag: |tag| tag == "pre",
            ..Default::default()
        }),
    );
    if let Some(TemplateChildNode::Element(element_after_pre)) = ast.inner.children.get(1) {
        // should not affect the <span> and condense its whitespace inside
        if let Some(TemplateChildNode::Text(node)) = element_after_pre.children().first() {
            assert_eq!(node.inner.content, " foo bar");
        } else {
            panic!()
        };
    } else {
        panic!()
    };
}

#[test]
/// should NOT condense whitespaces in RCDATA text mode
fn should_not_condense_whitespaces_in_rcdata_text_mode() {
    let ast = Parser::base_parse(
        "<textarea>Text:\n   foo</textarea>",
        Some(ParserOptions {
            get_text_mode: |el, _| {
                if el.tag() == "textarea" {
                    TextMode::RcData
                } else {
                    TextMode::Data
                }
            },
            ..Default::default()
        }),
    );

    if let Some(TemplateChildNode::Element(textarea_element)) = ast.inner.children.first() {
        assert_eq!(textarea_element.children().len(), 1);
        if let Some(TemplateChildNode::Text(node)) = textarea_element.children().first() {
            assert_eq!(node.inner.content, "Text:\n   foo");
        } else {
            panic!()
        };
    } else {
        panic!()
    };
}
