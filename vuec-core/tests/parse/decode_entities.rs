use vuec_core::{ast::template_child::TemplateChildNode, options::ParserOptions, parse::Parser};

#[test]
fn use_default_map() {
    let ast = Parser::base_parse("&gt;&lt;&amp;&apos;&quot;&foo;", None);
    assert_eq!(ast.inner.children.len(), 1);

    if let Some(TemplateChildNode::Text(node)) = ast.inner.children.get(0) {
        assert_eq!(node.inner.content, r#"><&'"&foo;"#);
    } else {
        panic!()
    }
}

#[test]
fn use_given_map() {
    let ast = Parser::base_parse(
        "&amp;&cups;",
        Some(ParserOptions {
            decode_entities: |raw, _| raw.replace("&cups;", r"\u222A\uFE00"),
            ..Default::default()
        }),
    );
    assert_eq!(ast.inner.children.len(), 1);

    if let Some(TemplateChildNode::Text(node)) = ast.inner.children.get(0) {
        assert_eq!(node.inner.content, r#"&amp;\u222A\uFE00"#);
    } else {
        panic!()
    }
}
