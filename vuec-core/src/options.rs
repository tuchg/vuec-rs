use std::collections::HashMap;

use regex::Captures;

use crate::{
    ast::{el::ElementNode, parent::ParentNode, NameSpace},
    compat::options::CompilerCompactOptions,
    errors::{default_on_error, default_on_warn, CompilerError},
    parse::TextMode,
    re::DECODE_RE,
    runtime_helpers::RuntimeHelper,
    transform::{DirectiveTransform, HoistTransform, NodeTransform},
    __DEV__,
};

pub struct ErrorHandlingOptions {
    pub on_warn: fn(warning: CompilerError),
    pub on_error: fn(error: CompilerError),
}

impl Default for ErrorHandlingOptions {
    fn default() -> Self {
        Self {
            on_warn: default_on_warn,
            on_error: default_on_error,
        }
    }
}

pub struct ParserOptions {
    /// Whether to keep comments in the templates AST.
    /// This defaults to `true` in development and `false` in production builds.
    pub comments: bool,
    /// @default ['{{', '}}']
    pub delimiters: (&'static str, &'static str),
    /// Whitespace handling strategy
    pub whitespace: WhiteSpaceStrategy,
    /// e.g. platform native elements, e.g. `<div>` for browsers
    pub is_native_tag: Option<fn(tag: &str) -> bool>,
    /// e.g. native elements that can self-close, e.g. `<img>`, `<br>`, `<hr>`
    pub is_void_tag: fn(tag: &str) -> bool,
    /// e.g. elements that should preserve whitespace inside, e.g. `<pre>`
    pub is_pre_tag: fn(tag: &str) -> bool,
    /// Platform-specific built-in components e.g. `<Transition>`
    pub is_builtin_component: Option<fn(tag: &str) -> Option<RuntimeHelper>>,
    /// Separate option for end users to extend the native elements list
    pub is_custom_element: fn(tag: &str) -> bool,
    /// Get namespace of an element
    pub get_namespace: fn(tag: &str, parent: Option<&ElementNode>) -> NameSpace,
    /// Get text parsing mode for this element
    pub get_text_mode: fn(tag: &ElementNode, parent: Option<&ElementNode>) -> TextMode,
    /// Only needed for DOM compilers
    pub decode_entities: fn(raw_text: &str, as_attr: bool) -> String,

    pub compact_options: CompilerCompactOptions,
    pub error_handling: ErrorHandlingOptions,
}

pub enum WhiteSpaceStrategy {
    /// Preserve all whitespace
    Preserve,
    /// Remove whitespace except in text nodes
    Condense,
}

pub enum BindingType {
    /// returned from data()
    Data,
    /// declared as a prop
    Props,
    /// a local alias of a `<script setup>` destructured prop.
    /// the original is stored in __propsAliases of the binding_metadata object.
    PropsAliased,
    /// a let binding (may or may not be a ref)
    SetupLet,
    /// a const binding that can never be a ref.
    /// these bindings don't need `unref()` calls when processed in inlined
    /// template expressions.
    SetupConst,
    /// a const binding that does not need `unref()`, but may be mutated.
    SetupReactiveConst,
    /// a const binding that may be a ref.
    SetupMaybeRef,
    /// bindings that are guaranteed to be refs
    SetupRef,
    /// declared by other options, e.g. computed, inject
    Options,
}

// TODO: BindingMetadata
type BindingMetadata = ();

#[derive(Clone)]
pub struct SharedTransformCodegenOptions {
    /// Transform expressions like {{ foo }} to `_ctx.foo`.
    /// If this option is false, the generated code will be wrapped in a
    /// `with (this) { ... }` block.
    /// - This is force-enabled in module mode, since modules are by default strict
    /// and cannot use `with`
    /// @default mode === 'module'
    pub prefix_identifiers: bool,
    /// Control whether generate SSR-optimized render functions instead.
    /// The resulting function must be attached to the component via the
    /// `ssrRender` option instead of `render`.
    ///
    /// When compiler generates code for SSR's fallback branch, we need to set it to false:
    ///  - context.ssr = false
    ///
    /// see `subTransform` in `ssrTransformComponent.ts`
    pub ssr: bool,
    /// Indicates whether the compiler generates code for SSR,
    /// it is always true when generating code for SSR,
    /// regardless of whether we are generating code for SSR's fallback branch,
    /// this means that when the compiler generates code for SSR's fallback branch:
    ///  - context.ssr = false
    ///  - context.in_ssr = true
    pub in_ssr: bool,
    /// Optional binding metadata analyzed from script - used to optimize
    /// binding access when `prefix_identifiers` is enabled.
    pub binding_metadata: Option<BindingMetadata>,
    /// Compile the function for inlining inside setup().
    /// This allows the function to directly access setup() local bindings.
    pub inline: bool,
    /// Indicates that transforms and codegen should try to output valid TS code
    pub is_ts: bool,
    /// Filename for source map generation.
    /// Also used for self-recursive reference in templates
    /// @default 'template.vue.html'
    pub filename: String,
}

impl Default for SharedTransformCodegenOptions {
    fn default() -> Self {
        Self {
            prefix_identifiers: false,
            ssr: false,
            in_ssr: false,
            binding_metadata: None,
            inline: false,
            is_ts: false,
            filename: "template.vue.html".to_string(),
        }
    }
}

pub struct TransformOptions {
    /// Hoist static VNodes and props objects to `_hoisted_x` constants
    /// @default false
    pub hoist_static: bool,
    /// Cache v-on handlers to avoid creating new inline functions on each render,
    /// also avoids the need for dynamically patching the handlers by wrapping it.
    /// e.g `@click="foo"` by default is compiled to `{ onClick: foo }`. With this
    /// option it's compiled to:
    /// ```js
    /// { onClick: _cache[0] || (_cache[0] = e => _ctx.foo(e)) }
    /// ```
    /// - Requires "prefix_identifiers" to be enabled because it relies on scope
    /// analysis to determine if a handler is safe to cache.
    /// @default false
    pub cache_handlers: bool,
    // /**
    //  * A list of parser plugins to enable for `@babel/parser`, which is used to
    //  * parse expressions in bindings and interpolations.
    //  * https://babeljs.io/docs/en/next/babel-parser#plugins
    //  */
    // expression_plugins ?: ParserPlugin[]
    /// SFC scoped styles ID
    pub scope_id: Option<String>,
    /// Indicates this SFC template has used :slotted in its styles
    /// Defaults to `true` for backwards compatibility - SFC tooling should set it
    /// to `false` if no `:slotted` usage is detected in `<style>`
    pub slotted: bool,
    /// SFC `<style vars>` injection string
    /// Should already be an object expression, e.g. `{ 'xxxx-color': color }`
    /// needed to render inline CSS variables on component root
    pub ssr_css_vars: Option<String>,
    pub is_builtin_component: Option<fn(tag: String) -> RuntimeHelper>,
    /// Used by some transforms that expects only native elements
    pub is_custom_element: Option<fn(tag: String) -> bool>,
    /// An array of node transforms to be applied to every AST node.
    pub node_transforms: Vec<NodeTransform>,
    /// An object of { name: transform } to be applied to every directive attribute
    /// node found on element nodes.
    pub directive_transforms: HashMap<String, DirectiveTransform>,
    /// An optional hook to transform a node being hoisted.
    /// used by compiler-dom to turn hoisted nodes into stringified HTML vnodes.
    /// @default null
    pub transform_hoist: Option<HoistTransform<ParentNode>>,
    /// If the pairing runtime provides additional built-in elements, use this to
    /// mark them as built-in so the compiler will generate component vnodes
    /// for them.
    pub compact_options: Option<CompilerCompactOptions>,
    pub error_handling: ErrorHandlingOptions,
    pub codegen_options: SharedTransformCodegenOptions,
}

#[derive(Clone)]
pub enum CodegenMode {
    /// `module` mode will generate ES module import statements for helpers
    /// and export the render function as the default export.
    Module,
    ///  `function` mode will generate a single `const { helpers... } = Vue`
    ///      statement and return the render function. It expects `Vue` to be globally
    ///     available (or passed by wrapping the code with an IIFE). It is meant to be
    ///     used with `new Function(code)()` to generate a render function at runtime.
    Function,
}

#[derive(Clone)]
pub struct CodegenOptions {
    /// - `module` mode will generate ES module import statements for helpers
    /// and export the render function as the default export.
    /// - `function` mode will generate a single `const { helpers... } = Vue`
    /// statement and return the render function. It expects `Vue` to be globally
    /// available (or passed by wrapping the code with an IIFE). It is meant to be
    /// used with `new Function(code)()` to generate a render function at runtime.
    /// @default 'function'
    pub mode: CodegenMode,
    /// Generate source map?
    /// @default false
    pub source_map: bool,
    /// SFC scoped styles ID
    pub scope_id: Option<String>,
    /// Option to optimize helper import bindings via variable assignment
    /// (only used for webpack code-split)
    /// @default false
    pub optimize_imports: bool,
    /// Customize where to import runtime helpers from.
    /// @default 'vue'
    pub runtime_module_name: String,
    /// Customize where to import ssr runtime helpers
    /// @default 'vue/server-renderer'
    pub ssr_runtime_module_name: String,
    /// Customize the global variable name of `Vue` to get helpers from
    /// in function mode
    /// @default 'Vue'
    pub runtime_global_name: String,

    pub inner: SharedTransformCodegenOptions,
}

impl Default for CodegenOptions {
    fn default() -> Self {
        Self {
            mode: CodegenMode::Function,
            source_map: false,
            scope_id: None,
            optimize_imports: false,
            runtime_module_name: "vue".to_string(),
            ssr_runtime_module_name: "vue/server-renderer".to_string(),
            runtime_global_name: "Vue".to_string(),
            inner: SharedTransformCodegenOptions::default(),
        }
    }
}

// TODO: reuse multiple options
// export type CompilerOptions = ParserOptions & TransformOptions & CodegenOptions
pub struct CompilerOptions {
    parser: ParserOptions,
    // transform: TransformOptions,
    codegen: CodegenOptions,
}

impl Default for ParserOptions {
    fn default() -> Self {
        Self {
            compact_options: Default::default(),
            error_handling: Default::default(),
            delimiters: ("{{", "}}"),
            is_void_tag: |_| false,
            is_pre_tag: |_| false,
            is_custom_element: |_| false,
            get_namespace: |_, _| NameSpace::Html,
            get_text_mode: |_, _| TextMode::Data,
            decode_entities: |raw, _| {
                // The default decoder only provides escapes for characters reserved as part of
                // the template syntax, and is only used if the custom renderer did not provide
                // a platform-specific decoder.
                DECODE_RE
                    .replace_all(raw, |caps: &Captures| match &caps[1] {
                        "lt" => "<",
                        "gt" => ">",
                        "quot" => "\"",
                        "amp" => "&",
                        "apos" => "'",
                        _ => "",
                    })
                    .to_string()
            },
            comments: __DEV__,
            is_native_tag: None,
            is_builtin_component: None,
            whitespace: WhiteSpaceStrategy::Condense,
        }
    }
}

impl Default for TransformOptions {
    fn default() -> Self {
        Self {
            is_builtin_component: None,
            is_custom_element: None,
            hoist_static: false,
            cache_handlers: false,
            scope_id: None,
            slotted: true,
            ssr_css_vars: None,
            node_transforms: vec![],
            directive_transforms: HashMap::new(),
            transform_hoist: None,
            compact_options: Default::default(),
            error_handling: Default::default(),
            codegen_options: SharedTransformCodegenOptions {
                prefix_identifiers: false,
                ssr: false,
                in_ssr: false,
                binding_metadata: None,
                inline: false,
                is_ts: false,
                filename: "".to_string(),
            },
        }
    }
}
