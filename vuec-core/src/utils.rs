use crate::{codegen::Assets, runtime_helpers::RuntimeHelper};

pub fn to_valid_asset_id(name: &str, kind: Assets) -> String {
    // see issue#4422, we need adding identifier on validAssetId if variable `name` has specific
    // character
    let kind: &str = kind.into();
    let name: String = name
        .chars()
        .filter_map(|c| match c {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => Some(c),
            '-' => Some('_'),
            _ => format!("{}", c as u32).chars().next(),
        })
        .collect();

    format!("_{kind}_{name}")
}

pub fn is_simple_identifier(name: &str) -> bool {
    let mut chars = name.chars();
    !(match chars.next() {
        Some(c) => c.is_numeric(),
        None => false,
    } || chars.any(|c| !c.is_ascii_alphanumeric() && c != '_' && c != '$'))
}

pub fn get_vnode_helper(ssr: bool, is_component: bool) -> RuntimeHelper {
    if ssr || is_component {
        RuntimeHelper::CreateVNode
    } else {
        RuntimeHelper::CreateElementVNode
    }
}

pub fn get_vnode_block_helper(ssr: bool, is_component: bool) -> RuntimeHelper {
    if ssr || is_component {
        RuntimeHelper::CreateBlock
    } else {
        RuntimeHelper::CreateElementBlock
    }
}

pub fn json_stringify(content: &str) -> String {
    format!("\"{}\"", content)
}
