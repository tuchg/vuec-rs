use std::{cell::RefCell, rc::Rc};

use ahash::AHashSet;

use crate::{
    ast::{
        attr::{AttributeValue, AttrsNode},
        el::ElementNode,
        expr::Expr,
        parent::Root,
        template_child::{Comment, Interpolation, TemplateChildNode, Text},
        utils::{Position, SourceLocation},
        ConstantType, ElementType, NameSpace, Node,
    },
    errors::{CompilerError, ErrorCode},
    options::{ParserOptions, WhiteSpaceStrategy},
    re::{
        ADVANCE_SPACE_RE, ATTR_NAME_RE, ATTR_VALUE_RE, ATTR_VALUE_SPACE_RE, COMMENT_RE,
        DIR_NAME_RE, DIR_RE, END_TAG_OPEN_RE, MISSING_SPACE_ATTRIBUTES_RE, TAG_NAME_RE,
        TAG_OPEN_RE, TEXT_RE1, TEXT_RE2, TEXT_RE3, TEXT_RE4, UNEXPECTED_CHARS_IN_UNQUOTED_RE,
        UNEXPECTED_CHAR_IN_ATTR_NAME_RE, UNQUOTED_RE,
    },
    runtime_helpers::RuntimeHelper,
};

/// |      | Elements | Entities | End sign              | Inside of |
/// | :--- | -------- | -------- | --------------------- | --------- |
/// | `DATA` | ✔        | ✔        | End tags of ancestors |           |
/// | `RCDATA` | ✘        |    ✔      |        End tag of the parent   | `<textarea>` |
/// | `RAWTEXT` | ✘        |     ✘      |     End tag of the parent  |`<style>`,`<script>` |
#[derive(Clone, Copy)]
pub enum TextMode {
    Data,
    RcData,
    RawText,
    CData,
    AttributeValue,
}

pub struct Parser {
    context: ParserContext,
    options: ParserOptions,
}

impl Parser {
    pub fn base_parse(content: &str, options: Option<ParserOptions>) -> Node<Root> {
        let options = options.unwrap_or(Default::default());
        let mut ctx = ParserContext::new(content.to_string(), options);

        let start = ctx.cursor();

        Node::<Root>::new(
            ctx.parse_children(&mut vec![], TextMode::Data),
            Some(ctx.selection(start, None)),
        )
    }
}

pub enum TagType {
    Start,
    End,
}

pub struct ParserContext {
    ///  HTML `<pre>` tag, preserve whitespaces
    in_pre: bool,
    /// v-pre, do not process directives and interpolations
    in_v_pre: bool,
    position: Position,
    source: String,
    original_source: String,
    options: ParserOptions,
}

impl ParserContext {
    fn new(content: String, options: ParserOptions) -> Self {
        ParserContext {
            options,
            original_source: content.clone(),
            source: content,
            position: Position {
                line: 1,
                column: 1,
                offset: 0,
            },
            in_pre: false,
            in_v_pre: false,
        }
    }

    #[inline]
    fn cursor(&self) -> Position {
        self.position
    }

    fn parse_children(
        &mut self,
        ancestors: &mut Vec<ElementNode>,
        mode: TextMode,
    ) -> Vec<TemplateChildNode> {
        let ns = if let Some(ancestor) = ancestors.last() {
            ancestor.ns()
        } else {
            NameSpace::Html
        };
        let mut nodes: Vec<TemplateChildNode> = vec![];

        while !self.is_end(mode, ancestors) {
            let source = self.source.as_str();
            let mut s_iter = source.chars();

            let mut node: Vec<TemplateChildNode> = vec![];

            if let TextMode::Data | TextMode::RcData = mode {
                if !self.in_v_pre && source.starts_with(self.options.delimiters.0) {
                    // '{{'
                    if let Some(interpolation) = self.parse_interpolation(mode) {
                        node.push(TemplateChildNode::new_interpolation(interpolation))
                    }
                } else if let TextMode::Data = mode && source.starts_with('<') {
                    let s_nth1 = s_iter.nth(1).unwrap();
                    // https://html.spec.whatwg.org/multipage/parsing.html#tag-open-state
                    if source.len() == 1 {
                        self.emit_error(ErrorCode::EOFBeforeTagName, Some(1), None);
                    } else if s_nth1 == '!' {
                        // https://html.spec.whatwg.org/multipage/parsing.html#markup-declaration-open-state
                        if source.starts_with("<!--") {
                            node.push(TemplateChildNode::new_comment(self.parse_comment()));
                        } else if source.starts_with("<!DOCTYPE") {
                            // Ignore DOCTYPE by a limitation.
                            node.push(TemplateChildNode::new_comment(self.parse_bogus_comment()));
                        } else if source.starts_with("<![CDATA[") {
                            if let NameSpace::Html = ns {
                                node = self.parse_cdata(ancestors);
                            } else {
                                self.emit_error(ErrorCode::CDataInHtmlContent, None, None);
                                node.push(TemplateChildNode::new_comment(self.parse_bogus_comment()));
                            }
                        } else {
                            self.emit_error(ErrorCode::IncorrectlyOpenedComment, None, None);
                            node.push(TemplateChildNode::new_comment(self.parse_bogus_comment()));
                        }
                    } else if s_nth1 == '/' {
                        //https://html.spec.whatwg.org/multipage/parsing.html#end-tag-open-state
                        let s_nth2 = s_iter.nth(2).unwrap();
                        if source.len() == 2 {
                            self.emit_error(ErrorCode::EOFBeforeTagName, Some(2), None);
                        } else if s_nth2 == '>' {
                            self.emit_error(ErrorCode::MissingEndTagName, Some(2), None);
                            self.advance_by(3);
                            continue;
                        } else if s_nth2.is_ascii_alphabetic() {
                            self.emit_error(ErrorCode::XInvalidEndTag, None, None);
                            self.parse_tag(TagType::End, ancestors.last().cloned());
                            continue;
                        } else {
                            self.emit_error(
                                ErrorCode::InvalidFirstCharacterOfTagName,
                                Some(2),
                                None,
                            );
                            node.push(TemplateChildNode::new_comment(self.parse_bogus_comment()));
                        }
                    } else if s_nth1.is_ascii_alphabetic() {
                        if let Some(el) = self.parse_element(ancestors) {
                            node.push(TemplateChildNode::new_el(el));
                        }
                        // todo! vue2
                        // 2.x <template> with no directive compat
                    } else if s_nth1 == '?' {
                        self.emit_error(
                            ErrorCode::UnexpectedQuestionMarkInsteadOfTagName,
                            Some(1),
                            None,
                        );
                        node.push(TemplateChildNode::new_comment(self.parse_bogus_comment()));
                    } else {
                        self.emit_error(ErrorCode::InvalidFirstCharacterOfTagName, Some(1), None);
                    }
                }
            }
            if node.is_empty() {
                node.push(TemplateChildNode::new_text(self.parse_text(mode)));
            }

            for node in node {
                push_node(&mut nodes, node);
            }
        }
        // Whitespace handling strategy like v2
        let mut removed_whitespace = vec![];
        match mode {
            TextMode::RawText | TextMode::RcData => {}
            _ => {
                let should_condense =
                    !matches!(self.options.whitespace, WhiteSpaceStrategy::Preserve);
                let nodes_len = nodes.len();

                for i in 0..nodes_len {
                    let node = &nodes[i];
                    if let TemplateChildNode::Text(text) = node {
                        let mut text = text.clone();
                        if !self.in_pre {
                            if !TEXT_RE1
                                .is_match(&text.inner.content)
                            {
                                // Remove if:
                                // - the whitespace is the first or last node, or:
                                // - (condense mode) the whitespace is between twos comments,
                                //   or:
                                // - (condense mode) the whitespace is between comment and
                                //   element, or:
                                // - (condense mode) the whitespace is between two elements AND
                                //   contains newline
                                // todo: bad code
                                let should_remove = if !(i >= 1 && i < nodes_len) {
                                    true
                                } else {
                                    match (nodes.get(i - 1), nodes.get(i + 1)) {
                                        (Some(prev), Some(next)) => {
                                            if should_condense {
                                                match (prev, next) {
                                                    (TemplateChildNode::Comment(_),
                                                        TemplateChildNode::Comment(_))
                                                    | (TemplateChildNode::Comment(_),
                                                        TemplateChildNode::Element(_))
                                                    | (TemplateChildNode::Element(_),
                                                        TemplateChildNode::Comment(_))
                                                    => { true }
                                                    (TemplateChildNode::Element(_),
                                                        TemplateChildNode::Element(_)) if
                                                    TEXT_RE2.is_match(&text.inner.content)
                                                    => { true }
                                                    (_, _) => { false }
                                                }
                                            } else { false }
                                        }
                                        (_, _) => { true }
                                    }
                                };

                                if should_remove {
                                    removed_whitespace.push(i);
                                } else {
                                    text.inner.content = " ".to_string()
                                }
                            } else if should_condense {
                                // in condense mode, consecutive whitespaces in text are
                                // condensed
                                // down to a single space.

                                text.inner.content = TEXT_RE3
                                    .replace_all(&text.inner.content, " ")
                                    .to_string();
                            }
                        } else {
                            // #6410 normalize windows newlines in <pre>:
                            // in SSR, browsers normalize server-rendered \r\n into a single \n
                            // in the DOM
                            text.inner.content = text.inner.content.replace("\r\n", "\n")
                        }
                        // sync change of text_node
                        nodes[i] = TemplateChildNode::Text(text);
                    } else if let TemplateChildNode::Comment(_) = node && !self.options.comments {
                        // Remove comment nodes if desired by configuration.
                        removed_whitespace.push(i)
                    }
                }

                if self.in_pre && let Some(parent) = ancestors.last() &&
                    self.options.is_pre_tag.call((parent.tag(), )) {
                    // remove leading newline per html spec
                    // https://html.spec.whatwg.org/multipage/grouping-content.html#the-pre-element// remove leading newline
                    if let Some(TemplateChildNode::Text(first)) = nodes.first_mut() {
                        first.inner.content = TEXT_RE4.replace_all(&first.inner.content, "").to_string();
                    }
                }
            }
        }

        nodes
            .into_iter()
            .enumerate()
            .filter(|(i, _)| !removed_whitespace.contains(i))
            .map(|(_, node)| node)
            .collect::<Vec<TemplateChildNode>>()
    }

    fn parse_element(&mut self, ancestors: &mut Vec<ElementNode>) -> Option<ElementNode> {
        // Start tag.
        let was_in_pre = self.in_pre;
        let was_in_v_pre = self.in_v_pre;

        let el_rc = Rc::new(RefCell::new(
            self.parse_tag(TagType::Start, ancestors.last().cloned())
                .unwrap(),
        ));

        let is_pre_boundary = self.in_pre && !was_in_pre;
        let is_v_pre_boundary = self.in_v_pre && !was_in_v_pre;

        // let mut ancestors = ancestors.clone();
        if el_rc.borrow().is_self_closing()
            || self.options.is_void_tag.call_once((el_rc.borrow().tag(),))
        {
            // #4030 self-closing <pre> tag
            if is_pre_boundary {
                self.in_pre = false;
            }
            if is_v_pre_boundary {
                self.in_v_pre = false;
            }
            return Some(el_rc.borrow().to_owned());
        }

        // Children.

        ancestors.push(el_rc.borrow().to_owned());

        let mode = self
            .options
            .get_text_mode
            .call_once((&el_rc.borrow(), ancestors.last().cloned()));

        let children = self.parse_children(ancestors, mode);
        ancestors.pop();

        // 2.x inline-template compat
        // todo! vue2

        let el_rc1 = el_rc.clone();

        let mut element = el_rc1.borrow_mut();

        // End tag.
        if self.starts_with_end_tag_open(element.tag()) {
            self.parse_tag(TagType::End, ancestors.last().cloned());
        } else {
            self.emit_error(
                ErrorCode::XMissingEndTag,
                Some(0),
                Some(element.loc().start),
            );

            if self.source.is_empty() &&
                el_rc.borrow().tag().eq_ignore_ascii_case("script") &&
                let Some(first) = children.first() &&
                first.loc().source.starts_with("<!==")
            {
                self.emit_error(ErrorCode::EOFInScriptHtmlCommentLikeText, None, None);
            }
        }

        *element.children_mut() = children;

        let loc = element.loc_mut();

        element.set_loc(self.selection(loc.start, None));

        if is_pre_boundary {
            self.in_pre = false;
        }
        if is_v_pre_boundary {
            self.in_v_pre = false;
        }

        Some(element.to_owned())
    }

    fn parse_comment(&mut self) -> Node<Comment> {
        let start = self.cursor();
        let content: String;

        let s = &self.source.clone();
        let mut matches = COMMENT_RE.find_iter(s);
        if let Some(matched) = matches.next() {
            // Regular comment.
            if matched.start() <= 3 {
                self.emit_error(ErrorCode::AbruptClosingOfEmptyComment, None, None);
            }
            if matches.next().is_some() {
                self.emit_error(ErrorCode::IncorrectlyClosedComment, None, None);
            }
            content = self.source[4..matched.start()].to_string();

            // Advancing with reporting nested comments.
            let s = self.source[0..matched.start()].to_string();

            let mut prev_index = 1;

            for i in 0..s.len() {
                if s[prev_index..].starts_with("<!--") {
                    self.advance_by(i - prev_index + 1);
                    if i + 4 < s.len() {
                        self.emit_error(ErrorCode::NestedComment, None, None);
                    }
                    prev_index = i + 1;
                }
            }

            self.advance_by(matched.end() - prev_index + 1);
        } else {
            content = self.source[4..].to_string();
            self.advance_by(self.source.len());
            self.emit_error(ErrorCode::EOFInComment, None, None);
        }

        Node::<Comment>::new(content, self.selection(start, None))
    }

    fn parse_text(&mut self, mode: TextMode) -> Node<Text> {
        let end_tokens = if let TextMode::CData = mode {
            vec!["]]>"]
        } else {
            vec!["<", self.options.delimiters.0]
        };
        let s_len = self.source.len();
        let mut end_index = s_len;
        for end_token in end_tokens {
            for i in 1..s_len {
                if self.source[i..].starts_with(end_token) && end_index > i {
                    end_index = i;
                    break;
                }
            }
        }
        let start = self.cursor();
        let content = self.parse_text_data(end_index, mode);

        Node::<Text>::new(content, self.selection(start, None))
    }

    fn parse_bogus_comment(&mut self) -> Node<Comment> {
        let start = self.cursor();
        let content_start = if self.source.chars().next().is_some() {
            1
        } else {
            2
        };
        let content: String;

        if let Some(close_index) = self.source.find('>') {
            content = self.source[content_start..close_index].to_string();
            self.advance_by(close_index + 1);
        } else {
            content = self.source[content_start..].to_string();
            self.advance_by(self.source.len());
        }

        Node::<Comment>::new(content, self.selection(start, None))
    }

    fn parse_interpolation(&mut self, mode: TextMode) -> Option<Node<Interpolation>> {
        let (open, close) = self.options.delimiters;
        // /// __TEST__
        // assert!(self.source.starts_with(open));
        // TODO: perf less 2 characters than ts version

        let close_index = if let Some(i) = self.source.find(close) {
            i
        } else {
            self.emit_error(ErrorCode::XMissingInterpolationEnd, None, None);
            return None;
        };

        let start = self.cursor();
        self.advance_by(open.len());

        let mut inner_start = self.cursor();
        let mut inner_end = self.cursor();

        let raw_content_length = close_index - open.len();
        let raw_content = self.source[..raw_content_length].to_string();
        let pre_trim_content = self.parse_text_data(raw_content_length, mode);
        let content = pre_trim_content.trim();

        let start_offset = if let Some(start_offset) = pre_trim_content.find(content) {
            inner_start.advance_position_with_mutation(&raw_content, start_offset);
            start_offset
        } else {
            0
        };

        let end_offset =
            raw_content_length - (pre_trim_content.len() - content.len() - start_offset);
        inner_end.advance_position_with_mutation(&raw_content, end_offset);
        self.advance_by(close.len());

        Some(Node::<Interpolation>::new(
            Node::new_simple_expr(
                content.to_string(),
                false,
                ConstantType::NotConstant,
                self.selection(inner_start, Some(inner_end)),
            ),
            self.selection(start, None),
        ))
    }

    fn parse_cdata(&mut self, ancestors: &mut Vec<ElementNode>) -> Vec<TemplateChildNode> {
        self.advance_by(9);
        let nodes = self.parse_children(ancestors, TextMode::CData);
        if self.source.is_empty() {
            self.emit_error(ErrorCode::EOFInCData, None, None);
        } else {
            self.advance_by(3);
        }
        nodes
    }

    /// Get text data with a given length from the current location.
    /// This translates HTML entities in the text data.
    fn parse_text_data(&mut self, n: usize, mode: TextMode) -> String {
        let raw_text = self.source[0..n].to_string();
        self.advance_by(n);
        if matches!(mode, TextMode::RawText | TextMode::CData) || !raw_text.contains('&') {
            raw_text
        } else {
            // DATA or RCDATA containing "&"". Entity decoding required.
            self.options
                .decode_entities
                .call_once((raw_text, matches!(mode, TextMode::AttributeValue)))
        }
    }

    /// Parse a tag (E.g. `<div id=a>`) with that type (start tag or end tag).
    fn parse_tag(&mut self, tag_type: TagType, parent: Option<ElementNode>) -> Option<ElementNode> {
        // Tag open.
        let start = self.cursor();
        let matches = TAG_OPEN_RE.captures(&self.source).unwrap();
        let match0 = matches.get(0).unwrap();
        let match1 = matches.get(1).unwrap();
        let tag: String = match1.as_str().to_string();
        let ns = self.options.get_namespace.call_once((&tag, parent));

        self.advance_by(match0.end());
        self.advance_spaces();

        // save current state in case we need to re-parse attributes with v-pre
        let cursor = self.cursor();
        let curr_source = self.source.clone();

        // check <pre> tag
        if self.options.is_pre_tag.call((&tag,)) {
            self.in_pre = true;
        }

        // Attributes.
        let mut props = self.parse_attributes(&tag_type);
        let pre_in_props = props.iter().any(|prop| {
            if let AttrsNode::Dir(node) = &prop && node.inner.name == "pre" {
                true
            } else { false }
        });
        // check v-pre
        if let TagType::Start = tag_type && !self.in_v_pre && pre_in_props {
            self.in_v_pre = true;
            //reset context
            self.position = cursor;
            self.source = curr_source;

            // re-parse attrs and filter out v-pre itself
            props = self.parse_attributes(&tag_type)
                .drain_filter(|p| p.name() != "v-pre")
                .collect::<Vec<AttrsNode>>()
        }

        // Tag close.
        let mut is_self_closing = false;
        if self.source.is_empty() {
            self.emit_error(ErrorCode::EOFInTag, None, None);
        } else {
            is_self_closing = self.source.starts_with("/>");
            if let TagType::End = tag_type && is_self_closing {
                self.emit_error(ErrorCode::EndTagWithTrailingSolidus, None, None);
            }
            self.advance_by(if is_self_closing { 2 } else { 1 });
        }

        if let TagType::End = tag_type {
            return None;
        }
        // 2.x deprecation checks
        // TODO vue2!

        let tag_type = if !self.in_v_pre {
            if tag == "slot" {
                ElementType::Slot
            } else if tag == "template" && props.iter().any(|p| if let
                AttrsNode::Dir(node) = &p && is_special_template_directive(&node.inner.name) { true } else {
                false
            }) {
                ElementType::Template
            } else if self.is_component(&tag, &props) {
                ElementType::Component
            } else {
                ElementType::Element
            }
        } else {
            ElementType::Element
        };

        Some(ElementNode::new(
            tag_type,
            ns,
            tag,
            is_self_closing,
            props,
            vec![],
            self.selection(start, None),
        ))
    }

    fn parse_attributes(&mut self, tag_type: &TagType) -> Vec<AttrsNode> {
        let mut props: Vec<AttrsNode> = vec![];
        let mut attr_names: AHashSet<String> = AHashSet::default();
        while !self.source.is_empty()
            && !self.source.starts_with('>')
            && !self.source.starts_with("/>")
        {
            if self.source.starts_with('/') {
                self.emit_error(ErrorCode::UnexpectedSolidusInTag, None, None);
                self.advance_by(1);
                self.advance_spaces();
                continue;
            }
            if let TagType::End = tag_type {
                self.emit_error(ErrorCode::EndTagWithAttributes, None, None);
            }

            let mut attr = self.parse_attribute(&mut attr_names);

            // Trim whitespace between class
            // https://github.com/vuejs/core/issues/4251
            if let AttrsNode::Attr(attr) = &mut attr {
                if let Some(attr_value) = &mut attr.inner.value && attr.inner.name == "class" {
                    attr_value.inner.content = ATTR_VALUE_SPACE_RE
                        .replace_all(&attr_value.inner.content, " ")
                        .trim()
                        .to_string();
                }
            }

            if let TagType::Start = tag_type {
                props.push(attr)
            }

            if MISSING_SPACE_ATTRIBUTES_RE.is_match(&self.source) {
                self.emit_error(ErrorCode::MissingWhitespaceBetweenAttributes, None, None);
            }

            self.advance_spaces();
        }
        props
    }

    fn parse_attribute(&mut self, attr_names: &mut AHashSet<String>) -> AttrsNode {
        // Name.
        let start = self.cursor();
        let source = &self.source.clone();

        let matched_name = ATTR_NAME_RE.find(source);

        let name = matched_name.unwrap().as_str();
        if attr_names.get(name).is_some() {
            self.emit_error(ErrorCode::DuplicateAttribute, None, None);
        }
        attr_names.insert(name.to_string());

        if name.starts_with('=') {
            self.emit_error(
                ErrorCode::UnexpectedEqualsSignBeforeAttributeName,
                None,
                None,
            );
        }

        UNEXPECTED_CHAR_IN_ATTR_NAME_RE
            .captures_iter(name)
            .for_each(|caps| {
                caps.iter().for_each(|matched| {
                    if let Some(matched) = matched {
                        self.emit_error(
                            ErrorCode::UnexpectedCharacterInAttributeName,
                            Some(matched.start()),
                            None,
                        );
                    }
                })
            });

        self.advance_by(name.len());

        // Value.
        let mut value = None;

        if ATTR_VALUE_RE.is_match(self.source.as_str()) {
            self.advance_spaces();
            self.advance_by(1);
            self.advance_spaces();
            let temp_value = self.parse_attribute_value();
            if temp_value.is_none() {
                self.emit_error(ErrorCode::MissingAttributeValue, None, None);
            } else {
                value = temp_value;
            }
        };

        let loc = self.selection(start, None);

        if !self.in_v_pre && DIR_RE.is_match(name) {
            let matched = DIR_NAME_RE.captures(name).unwrap();
            let is_prop_shorthand = name.starts_with('.');
            let dir_name = if let Some(dir_name) = matched.get(1) {
                dir_name.as_str()
            } else if is_prop_shorthand || name.starts_with(':') {
                "bind"
            } else if name.starts_with('@') {
                "on"
            } else {
                "slot"
            };
            let mut arg: Option<Node<Expr>> = None;

            let matched3 = matched.get(3);

            if let Some(matched2) = matched.get(2) {
                let is_slot = dir_name == "slot";
                let start_offset = name.find(matched2.as_str()).unwrap();

                let n = start_offset
                    + matched2.range().len()
                    + if is_slot {
                        if let Some(matched3) = matched3 {
                            matched3.end()
                        } else {
                            0
                        }
                    } else {
                        0
                    };

                let loc = self.selection(
                    self.get_new_position(start, start_offset),
                    Some(self.get_new_position(start, n)),
                );

                let mut content = matched2.as_str().to_string();
                let mut is_static = true;

                if content.starts_with('[') {
                    is_static = false;
                    if !content.ends_with(']') {
                        self.emit_error(ErrorCode::XMissingDynamicDirectiveArgumentEnd, None, None);
                        content = content[1..].to_string();
                    } else {
                        content = content[1..content.len() - 1].to_string();
                    }
                } else if is_slot {
                    // #1241 special case for v-slot: vuetify relies extensively on slot
                    // names containing dots. v-slot doesn't have any modifiers and Vue 2.x
                    // supports such usage so we are keeping it consistent with 2.x.

                    content += if let Some(matched3) = matched3 {
                        matched3.as_str()
                    } else {
                        ""
                    };
                }

                arg = Some(Node::<Expr>::new_simple_expr(
                    content,
                    is_static,
                    if is_static {
                        ConstantType::CanStringify
                    } else {
                        ConstantType::NotConstant
                    },
                    loc,
                ));
            }

            if let Some(value) = &mut value && value.inner.is_quoted {
                let value_loc = &mut value.loc;
                value_loc.start.offset += 1;
                value_loc.start.column += 1;
                value_loc.end = value_loc.start.advance_position_with_clone(&value.inner.content,
                                                                            None);
                value_loc.source = value_loc.source[1..&value_loc.source.len() - 1].to_string();
            }
            let mut modifiers = if let Some(matched3) = matched3 {
                matched3.as_str()[1..]
                    .split('.')
                    .map(|s| s.to_string())
                    .collect()
            } else {
                vec![]
            };

            if is_prop_shorthand {
                modifiers.push("prop".to_string());
            }
            // 2.x compat v-bind:foo.sync -> v-model:foo
            // TODO

            return AttrsNode::new_dir(
                dir_name.to_string(),
                arg,
                value.map(|value| {
                    Node::new_simple_expr(
                        value.inner.content,
                        false,
                        // Treat as non-constant by default. This can be potentially set to
                        // other values by `transformExpression` to make it eligible for hoisting.
                        ConstantType::NotConstant,
                        value.loc,
                    )
                }),
                modifiers,
                loc,
            );
        }

        // missing directive name or illegal directive name
        if !self.in_v_pre && name.starts_with("v-") {
            self.emit_error(ErrorCode::XMissingDirectiveName, None, None);
        }

        AttrsNode::new_attr(
            name.to_string(),
            value.map(|value| Node::<Text>::new(value.inner.content, value.loc)),
            loc,
        )
    }

    fn get_new_position(&self, start: Position, n: usize) -> Position {
        let source = if start.offset <= n {
            self.original_source[start.offset..n].to_string()
        } else {
            String::new()
        };
        start.advance_position_with_clone(&source, Some(n))
    }

    fn parse_attribute_value(&mut self) -> Option<Node<AttributeValue>> {
        let start = self.cursor();
        let content: String;

        let quote = self.source.chars().next().unwrap();
        let is_quoted = quote == '"' || quote == '\'';
        if is_quoted {
            // Quoted value.
            self.advance_by(1);

            if let Some(end_index) = self.source.find(quote) {
                content = self.parse_text_data(end_index, TextMode::AttributeValue);
                self.advance_by(1);
            } else {
                content = self.parse_text_data(self.source.len(), TextMode::AttributeValue);
            }
        } else {
            // Unquoted
            if let Some(matched) = UNQUOTED_RE.find(&self.source) {
                // unexpectedChars

                for cap in UNEXPECTED_CHARS_IN_UNQUOTED_RE.captures_iter(matched.as_str()) {
                    self.emit_error(
                        ErrorCode::UnexpectedCharacterInUnquotedAttributeValue,
                        cap.get(0).map(|m| m.start()),
                        None,
                    );
                }

                content = self.parse_text_data(matched.end(), TextMode::AttributeValue);
            } else {
                return None;
            }
        }

        Some(Node::<AttributeValue>::new(
            content,
            is_quoted,
            self.selection(start, None),
        ))
    }

    fn is_component(&self, tag: &String, props: &Vec<AttrsNode>) -> bool {
        let options = &self.options;
        if options.is_custom_element.call_once((tag,)) {
            return false;
        }

        if tag == "component"
            || TAG_NAME_RE.is_match(tag)
            || is_core_component(tag).is_some()
            || self
                .options
                .is_builtin_component
                .is_some_and(|f| f.call_once((tag,)).is_some())
            || self
                .options
                .is_native_tag
                .is_some_and(|f| !f.call_once((tag,)))
        {
            return true;
        }
        // at this point the tag should be a native tag, but check for potential "is"
        // casting
        for prop in props {
            match &prop {
                AttrsNode::Attr(attr) => {
                    if let Some(value) = &attr.inner.value && attr.inner.name == "is" {
                        if value.inner.content.starts_with("vue:") {
                            return true;
                        } else {
                            // todo!("vue2")
                        }
                    }
                }
                // directive
                AttrsNode::Dir(dir) => {
                    // v-is (TODO Deprecate)
                    if dir.inner.name == "is" {
                        return true;
                    } else {
                        // todo!("vue2")
                    }
                }
            }
        }
        false
    }

    fn advance_by(&mut self, n: usize) {
        let source = &self.source;
        // /// __TEST__
        // assert!(n <= s.len());
        self.position.advance_position_with_mutation(source, n);
        self.source = source[n..].to_string();
    }

    fn advance_spaces(&mut self) {
        if let Some(matched) = ADVANCE_SPACE_RE.find(&self.source) {
            self.advance_by(matched.end());
        }
    }

    fn selection(&self, start: Position, end: Option<Position>) -> SourceLocation {
        let end = end.unwrap_or(self.cursor());
        let source = self.original_source[start.offset..end.offset].to_string();

        SourceLocation { start, end, source }
    }

    fn emit_error(&self, code: ErrorCode, offset: Option<usize>, loc: Option<Position>) {
        let mut loc = loc.unwrap_or(self.cursor());
        if let Some(offset) = offset {
            loc.offset += offset;
            loc.column += offset;
        }

        self.options
            .error_handling
            .on_error
            .call_once((CompilerError::new(
                code,
                Some(SourceLocation {
                    start: loc,
                    end: loc,
                    source: "".to_string(),
                }),
            ),));
    }

    fn is_end(&self, mode: TextMode, ancestors: &[ElementNode]) -> bool {
        let source = &self.source;
        match mode {
            TextMode::RcData | TextMode::RawText => {
                if let Some(parent) = ancestors.last() {
                    if self.starts_with_end_tag_open(parent.tag()) {
                        return true;
                    }
                }
            }
            TextMode::Data if source.starts_with("</") => {
                // TODO: probably bad performance
                for ancestor in ancestors.iter().rev() {
                    if self.starts_with_end_tag_open(ancestor.tag()) {
                        return true;
                    }
                }
            }
            TextMode::CData if source.starts_with("[[>") => {
                return true;
            }
            _ => {}
        }
        source.is_empty()
    }

    fn starts_with_end_tag_open(&self, tag: &String) -> bool {
        let end_i = 2 + tag.len();
        let s_slice = &self.source[2..end_i];

        self.source.starts_with("</")
            && s_slice.eq_ignore_ascii_case(tag)
            && END_TAG_OPEN_RE.is_match(self.source.get(end_i..).unwrap_or(">"))
    }
}

pub fn is_special_template_directive(name: &str) -> bool {
    matches!(name, "if" | "else" | "else-if" | "for" | "slot")
}

pub fn is_core_component(tag: &str) -> Option<RuntimeHelper> {
    match tag.to_lowercase().as_str() {
        "teleport" => Some(RuntimeHelper::Teleport),
        "suspense" => Some(RuntimeHelper::Suspense),
        "keepalive" => Some(RuntimeHelper::KeepAlive),
        "basetransition" => Some(RuntimeHelper::BaseTransition),
        _ => None,
    }
}

pub fn push_node(nodes: &mut Vec<TemplateChildNode>, node: TemplateChildNode) {
    if let TemplateChildNode::Text(text) = &node {
        // Merge if both this and the previous node are text and those are
        // consecutive. This happens for cases like "a < b".
        if let Some(TemplateChildNode::Text(prev)) = nodes.last_mut() {
            if prev.loc.end.offset == text.loc.start.offset {
                prev.inner.content += &text.inner.content;
                prev.loc.end = text.loc.end;
                prev.loc.source += &text.loc.source;
                return;
            }
        }
    }
    nodes.push(node);
}
