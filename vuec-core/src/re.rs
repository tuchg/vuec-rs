use once_cell::sync::Lazy;
use regex::{Regex, RegexBuilder};

macro_rules! lazy_re {
    ($re_name:ident, $re:expr $(,)?) => {
        pub static $re_name: Lazy<Regex> = Lazy::new(|| $re.unwrap());
    };
}

/////   Parser REs.
lazy_re!(DECODE_RE, Regex::new(r"&(gt|lt|amp|apos|quot);"));
// lazy_re!(END_TAG_OPEN_RE, Regex::new(r"[\t\r\n\f />]"));
// lazy_re!(
//     TAG_OPEN_RE,
//     RegexBuilder::new(r#"^</?([a-z][^\t\r\n\f />]*)"#)
//         .case_insensitive(true)
//         .build()
// );
// lazy_re!(ADVANCE_SPACE_RE, Regex::new(r"^[\t\r\n\f ]+"));
lazy_re!(COMMENT_RE, Regex::new(r"--(!)?>"));
// lazy_re!(TEXT_RE1, Regex::new(r"[^\t\r\n\f ]"));
// lazy_re!(TEXT_RE2, Regex::new("[\r\n]"));
// lazy_re!(TEXT_RE3, Regex::new(r"[\t\r\n\f ]+"));
// lazy_re!(TEXT_RE4, Regex::new(r"^\r?\n"));
// lazy_re!(ATTR_VALUE_SPACE_RE, Regex::new(r"\s+"));
// lazy_re!(MISSING_SPACE_ATTRIBUTES_RE, Regex::new(r"^[^\t\r\n\f />]"));
// lazy_re!(ATTR_NAME_RE, Regex::new(r"^[^\t\r\n\f />][^\t\r\n\f />=]*"));
// lazy_re!(ATTR_VALUE_RE, Regex::new(r"^[\t\r\n\f ]*="));
lazy_re!(
    DIR_NAME_RE,
    RegexBuilder::new(r"(?:^v-([a-z0-9-]+))?(?:(?::|^\.|^@|^#)(\[[^\]]+\]|[^\.]+))?(.+)?$")
        .case_insensitive(true)
        .build()
);
// lazy_re!(DIR_RE, Regex::new(r"^(v-[A-Za-z0-9-]|:|\.|@|#)"));
lazy_re!(UNQUOTED_RE, Regex::new(r"^[^\t\r\n\f >]+"));
// lazy_re!(UNEXPECTED_CHARS_IN_UNQUOTED_RE, Regex::new("[\"'<=`]"));
// lazy_re!(UNEXPECTED_CHAR_IN_ATTR_NAME_RE, Regex::new("[\"'<]"));
// lazy_re!(TAG_NAME_RE, Regex::new(r"^[A-Z]"));

///// other REs

lazy_re!(NON_IDENTIFIER_RE, Regex::new(r"^\d|[^\$\w]"));
lazy_re!(
    VALID_FIRST_IDENT_CHAR_RE,
    Regex::new(r"[A-Za-z_$\xA0-\uFFFF]")
);
lazy_re!(VALID_IDENT_CHAR_RE, Regex::new(r"[\.\?\w$\xA0-\uFFFF]"));
lazy_re!(
    FOR_ALIAS_RE,
    Regex::new(r"([\s\S]*?)\s+(?:in|of)\s+([\s\S]*)")
);

/// This regex doesn't cover the case if key or index aliases have destructuring,
/// but those do not make sense in the first place, so this works in practice.
lazy_re!(
    FOR_ITERATOR_RE,
    Regex::new(r",([^,\}\]]*)(?:,([^,\}\]]*))?$")
);

lazy_re!(STRIP_PARENS_RE, Regex::new(r"^\(|\)$"));
lazy_re!(WHITESPACE_RE, Regex::new(r"[\\s\n]"));

lazy_re!(
    STRIP_STRING_RE,
    Regex::new(
        r#"'(?:[^'\\]|\\.)*'|"(?:[^"\\]|\\.)*"|`(?:[^`\\]|\\.)*\$\{|\}(?:[^`\\]|\\.)*`|`(?:[^`\\]|\\.)*`"#
    )
);
