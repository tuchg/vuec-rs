// whitespace management when adopting strategy preserve

use vuec_core::{
    ast::{parent::Root, template_child::TemplateChildNode, Node},
    options::{ParserOptions, WhiteSpaceStrategy},
    parse::Parser,
};

fn parse(content: &str, options: Option<ParserOptions>) -> Node<Root> {
    Parser::base_parse(
        content,
        Some(ParserOptions {
            whitespace: WhiteSpaceStrategy::Preserve,
            ..options.unwrap_or_default()
        }),
    )
}

#[test]
/// should still remove whitespaces at start/end inside an element
fn should_still_remove_whitespace_at_start_or_end_inside_an_element() {
    let ast = parse("<div>   <span/>    </div>", None);
    if let Some(TemplateChildNode::Element(el)) = ast.inner.children.first() {
        assert_eq!(el.children().len(), 1);
    } else {
        panic!()
    };
}

#[test]
/// should preserve whitespaces w/ newline between elements
fn should_preserve_whitespace_with_newline_between_elements() {
    let ast = parse("<div/> \n <div/> \n <div/>", None);

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
/// should preserve whitespaces adjacent to comments
fn should_preserve_whitespace_adjacent_to_comments() {
    let ast = parse("<div/> \n <!--foo--> <div/>", None);
    assert_eq!(ast.inner.children.len(), 5);
    assert!(matches!(
        ast.inner.children[0],
        TemplateChildNode::Element(_)
    ));
    assert!(matches!(ast.inner.children[1], TemplateChildNode::Text(_)));
    assert!(matches!(
        ast.inner.children[2],
        TemplateChildNode::Comment(_)
    ));
    assert!(matches!(ast.inner.children[3], TemplateChildNode::Text(_)));
    assert!(matches!(
        ast.inner.children[4],
        TemplateChildNode::Element(_)
    ));
}

#[test]
/// should preserve whitespaces w/ newline between comments and elements
fn should_preserve_whitespace_with_newline_between_comments_and_elements() {
    let ast = parse("<div/> \n <!--foo--> \n <div/>", None);
    assert_eq!(ast.inner.children.len(), 5);
    assert!(matches!(
        ast.inner.children[0],
        TemplateChildNode::Element(_)
    ));
    assert!(matches!(ast.inner.children[1], TemplateChildNode::Text(_)));
    assert!(matches!(
        ast.inner.children[2],
        TemplateChildNode::Comment(_)
    ));
    assert!(matches!(ast.inner.children[3], TemplateChildNode::Text(_)));
    assert!(matches!(
        ast.inner.children[4],
        TemplateChildNode::Element(_)
    ));
}

#[test]
/// should preserve whitespaces w/ newline between interpolations
fn should_preserve_whitespace_with_newline_between_interpolations() {
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
/// should preserve whitespaces w/o newline between elements
fn should_preserve_whitespace_without_newline_between_elements() {
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
/// should preserve consecutive whitespaces in text
fn should_preserve_consecutive_whitespace_in_text() {
    let content = "   foo  \n    bar     baz     ";
    let ast = parse(content, None);

    if let Some(TemplateChildNode::Text(text)) = ast.inner.children.first() {
        assert_eq!(text.inner.content, content);
    } else {
        panic!()
    };
}
