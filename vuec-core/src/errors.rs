use std::fmt::Display;

use crate::ast::utils::SourceLocation;

#[derive(Debug)]
pub struct CompilerError {
    pub code: ErrorCode,
    pub loc: Option<SourceLocation>,
}

impl CompilerError {
    pub fn new(code: ErrorCode, loc: Option<SourceLocation>) -> Self {
        Self { code, loc }
    }
    pub fn message(self) -> &'static str {
        self.code.into_message()
    }
}

pub fn default_on_error(err: CompilerError) {
    println!("{:?}", err);
}

pub fn default_on_warn(err: CompilerError) {
    println!("[Vue warn] {:?}", err);
}

#[derive(Debug, PartialEq, Eq)]
pub enum ErrorCode {
    /// parse errors
    AbruptClosingOfEmptyComment,
    CDataInHtmlContent,
    DuplicateAttribute,
    EndTagWithAttributes,
    EndTagWithTrailingSolidus,
    EOFBeforeTagName,
    EOFInCData,
    EOFInComment,
    EOFInScriptHtmlCommentLikeText,
    EOFInTag,
    IncorrectlyClosedComment,
    IncorrectlyOpenedComment,
    InvalidFirstCharacterOfTagName,
    MissingAttributeValue,
    MissingEndTagName,
    MissingWhitespaceBetweenAttributes,
    NestedComment,
    UnexpectedCharacterInAttributeName,
    UnexpectedCharacterInUnquotedAttributeValue,
    UnexpectedEqualsSignBeforeAttributeName,
    UnexpectedNullCharacter,
    UnexpectedQuestionMarkInsteadOfTagName,
    UnexpectedSolidusInTag,

    /// Vue-specific parse errors
    XInvalidEndTag,
    XMissingEndTag,
    XMissingInterpolationEnd,
    XMissingDirectiveName,
    XMissingDynamicDirectiveArgumentEnd,

    /// transform errors
    XVIfNoExpression,
    XVIfSameKey,
    XVElseNoAdjacentIf,
    XVForNoExpression,
    XVForMalformedExpression,
    XVForTemplateKeyPlacement,
    XVBindNoExpression,
    XVOnNoExpression,
    XVSlotUnexpectedDirectiveOnSlotOutlet,
    XVSlotMixedSlotUsage,
    XVSlotDuplicateSlotNames,
    XVSlotExtraneousDefaultSlotChildren,
    XVSlotMisplaced,
    XVModelNoExpression,
    XVModelMalformedExpression,
    XVModelOnScopeVariable,
    XVModelOnProps,
    XInvalidExpression,
    XKeepAliveInvalidChildren,

    /// generic errors
    XPrefixIdNotSupported,
    XModuleModeNotSupported,
    XCacheHandlerNotSupported,
    XScopeIdNotSupported,

    /// Special value for higher-order compilers to pick up the last code
    /// to avoid collision of error codes. This should always be kept as the last
    /// item.
    __ExtendPoint__,
}

impl Display for ErrorCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.into_message())
    }
}

impl ErrorCode {
    #[inline(never)]
    fn into_message(&self) -> &'static str {
        match &self {
            Self::AbruptClosingOfEmptyComment => "Illegal comment.",
            Self::CDataInHtmlContent => "CDATA section is allowed only in XML context.",
            Self::DuplicateAttribute => "Duplicate attribute.",
            Self::EndTagWithAttributes => "End tag cannot have attributes.",
            Self::EndTagWithTrailingSolidus => "Illegal '/' in tags.",
            Self::EOFBeforeTagName => "Unexpected EOF in tag.",
            Self::EOFInCData => "Unexpected EOF in CDATA section.",
            Self::EOFInComment => "Unexpected EOF in comment.",
            Self::EOFInScriptHtmlCommentLikeText => "Unexpected EOF in script.",
            Self::EOFInTag => "Unexpected EOF in tag.",
            Self::IncorrectlyClosedComment => "Incorrectly closed comment.",
            Self::IncorrectlyOpenedComment => "Incorrectly opened comment.",
            Self::InvalidFirstCharacterOfTagName => "Illegal tag name. Use '&lt;' to print '<'.",
            Self::MissingAttributeValue => "Attribute value was expected.",
            Self::MissingEndTagName => "End tag name was expected.",
            Self::MissingWhitespaceBetweenAttributes => "Whitespace was expected.",
            Self::NestedComment => "Unexpected '<!--' in comment.",
            Self::UnexpectedCharacterInAttributeName => {
                "Attribute name cannot contain U+0022 (\"), U+0027 ('), and U+003C (<)."
            }
            Self::UnexpectedCharacterInUnquotedAttributeValue => {
                "Unquoted attribute value cannot contain U+0022 (\"), U+0027 ('), U+003C (<), \
                 U+003D (=), and U+0060 (`)."
            }
            Self::UnexpectedEqualsSignBeforeAttributeName => {
                "Attribute name cannot start with '='."
            }
            Self::UnexpectedQuestionMarkInsteadOfTagName => "'<?' is allowed only in XML context.",
            Self::UnexpectedNullCharacter => "Unexpected null character.",
            Self::UnexpectedSolidusInTag => "Illegal '/' in tags.",
            Self::XInvalidEndTag => "Invalid end tag.",
            Self::XMissingEndTag => "End tag was not found.",
            Self::XMissingInterpolationEnd => "Interpolation end sign was not found.",
            Self::XMissingDynamicDirectiveArgumentEnd => {
                "End bracket for dynamic directive argument was not found.Note that dynamic \
                 directive argument cannot contain spaces."
            }
            Self::XMissingDirectiveName => "Legal directive name was expected.",
            Self::XVIfNoExpression => "v-if/v-else-if is missing expression.",
            Self::XVIfSameKey => "v-if/else branches must use unique keys.",
            Self::XVElseNoAdjacentIf => "v-else/v-else-if has no adjacent v-if.",
            Self::XVForNoExpression => "v-for is missing expression.",
            Self::XVForMalformedExpression => "v-for has invalid expression.",
            Self::XVForTemplateKeyPlacement => "v-for on <template> requires v-bind:key.",
            Self::XVBindNoExpression => "v-bind is missing expression.",
            Self::XVOnNoExpression => "v-on is missing expression.",
            Self::XVSlotUnexpectedDirectiveOnSlotOutlet => {
                "Unexpected custom directive on <slot> outlet."
            }
            Self::XVSlotMixedSlotUsage => {
                "Mixed v-slot usage on both the component and nested <template>.When there are \
                 multiple named slots, all slots should use <template> syntax to avoid scope \
                 ambiguity."
            }
            Self::XVSlotDuplicateSlotNames => "Duplicate slot names found.",
            Self::XVSlotExtraneousDefaultSlotChildren => {
                "Extraneous children found when component already has explicitly named slots. \
                 These children will be ignored."
            }
            Self::XVSlotMisplaced => "v-slot can only be used on components or <template>.",
            Self::XVModelNoExpression => "v-model is missing expression.",
            Self::XVModelMalformedExpression => {
                "v-model value must be a valid JavaScript member expression."
            }
            Self::XVModelOnScopeVariable => {
                "v-model cannot be used on v-for or v-slot scope variables because they are not \
                 writable."
            }
            Self::XVModelOnProps => {
                "v-model cannot be used on a prop, because local prop bindings are not writable.
Use a v-bind binding combined with a v-on listener that emits update:x event instead."
            }
            Self::XInvalidExpression => "Error parsing JavaScript expression.",
            Self::XKeepAliveInvalidChildren => "<KeepAlive> expects exactly one child component.",
            Self::XPrefixIdNotSupported => {
                "prefixIdentifiers option is not supported in this build of compiler."
            }
            Self::XModuleModeNotSupported => {
                "ES module mode is not supported in this build of compiler."
            }
            Self::XCacheHandlerNotSupported => {
                "cacheHandlers option is only supported when the prefixIdentifiers option is \
                 enabled."
            }
            Self::XScopeIdNotSupported => "scopeId option is only supported in module mode.",
            Self::__ExtendPoint__ => "",
        }
    }
}
