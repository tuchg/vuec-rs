use vuec_core::{
    ast::{
        el::ElementNode,
        template_child::{Comment, TemplateChildNode},
        utils::{Position, SourceLocation},
        Node,
    },
    options::ParserOptions,
    parse::Parser,
};

#[test]
fn empty_comment() {
    let ast = Parser::base_parse("<!---->", None);
    let comment = ast.inner.children.first().unwrap();

    assert_eq!(
        comment,
        &TemplateChildNode::new_comment(Node::<Comment>::new(
            "".to_string(),
            SourceLocation {
                start: Position {
                    offset: 0,
                    line: 1,
                    column: 1,
                },
                end: Position {
                    offset: 7,
                    line: 1,
                    column: 8,
                },
                source: "<!---->".to_string(),
            },
        ))
    );
}

#[test]
fn comments_option() {
    let ast_no_comments = Parser::base_parse(
        "<!--abc-->",
        Some(ParserOptions {
            comments: false,
            ..Default::default()
        }),
    );
    let ast_with_comments = Parser::base_parse(
        "<!--abc-->",
        Some(ParserOptions {
            comments: true,
            ..Default::default()
        }),
    );

    assert_eq!(ast_no_comments.inner.children.len(), 0);
    assert_eq!(ast_with_comments.inner.children.len(), 1);
}

#[test]
/// #2217
/// comments in the <pre> tag should be removed when comments option requires it
fn comments_in_pre_tag_should_removed() {
    let raw_text = "<p/><!-- foo --><p/>";
    let ast_with_comments = Parser::base_parse(
        &format!("<pre>{raw_text}</pre>"),
        Some(ParserOptions {
            comments: true,
            ..Default::default()
        }),
    );

    let children =
        if let Some(TemplateChildNode::Element(el)) = ast_with_comments.inner.children.first() {
            el.children()
        } else {
            panic!();
        };
    if let Some(TemplateChildNode::Element(ElementNode::Plain(el))) = children.get(0) {
        assert_eq!(el.inner.tag, "p");
    } else {
        panic!()
    }

    assert!(matches!(
        children.get(1).unwrap(),
        TemplateChildNode::Comment(_)
    ));

    if let Some(TemplateChildNode::Element(ElementNode::Plain(el))) = children.get(2) {
        assert_eq!(el.inner.tag, "p");
    } else {
        panic!()
    }

    let ast_without_comments = Parser::base_parse(
        &format!("<pre>{raw_text}</pre>"),
        Some(ParserOptions {
            comments: false,
            ..Default::default()
        }),
    );

    let children =
        if let Some(TemplateChildNode::Element(el)) = ast_without_comments.inner.children.first() {
            el.children()
        } else {
            panic!();
        };
    if let Some(TemplateChildNode::Element(ElementNode::Plain(el))) = children.get(0) {
        assert_eq!(el.inner.tag, "p");
    } else {
        panic!()
    }
    if let Some(TemplateChildNode::Element(ElementNode::Plain(el))) = children.get(1) {
        assert_eq!(el.inner.tag, "p");
    } else {
        panic!()
    }
}
