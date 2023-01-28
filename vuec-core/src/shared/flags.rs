/// Patch flags are optimization hints generated by the compiler.
/// when a block with dynamicChildren is encountered during diff, the algorithm
/// enters "optimized mode". In this mode, we know that the vdom is produced by
/// a render function generated by the compiler, so the algorithm only needs to
/// handle updates explicitly marked by these patch flags.
///
/// Patch flags can be combined using the | bitwise operator and can be checked
/// using the & operator, e.g.
///
/// ```js
/// const flag = TEXT | CLASS
/// if (flag & TEXT) { ... }
/// ```
///
/// Check the `patchElement` function in '../../runtime-core/src/renderer.ts' to see how the
/// flags are handled during diff.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct PatchFlags;

impl PatchFlags {
    /// Indicates an element with dynamic textContent (children fast path)
    const TEXT: i16 = 1;

    /// Indicates an element with dynamic class binding.
    const CLASS: i16 = 1 << 1;

    /// Indicates an element with dynamic style
    /// The compiler pre-compiles static string styles into static objects
    /// + detects and hoists inline static objects
    /// e.g. `style="color: red"` and `:style="{ color: 'red' }"` both get hoisted
    /// as:
    /// ```js
    /// const style = { color: 'red' }
    /// render() { return e('div', { style }) }
    /// ```
    const STYLE: i16 = 1 << 2;

    /// Indicates an element that has non-class/style dynamic props.
    /// Can also be on a component that has any dynamic props (includes
    /// class/style). when this flag is present, the vnode also has a dynamicProps
    /// array that contains the keys of the props that may change so the runtime
    /// can diff them faster (without having to worry about removed props)
    const PROPS: i16 = 1 << 3;
    /// Indicates an element with props with dynamic keys. When keys change, a full
    /// diff is always needed to remove the old key. This flag is mutually
    /// exclusive with CLASS, STYLE and PROPS.
    const FULL_PROPS: i16 = 1 << 4;
    /// Indicates an element with event listeners (which need to be attached
    /// during hydration)
    const HYDRATE_EVENTS: i16 = 1 << 5;
    /// Indicates a fragment whose children order doesn't change.
    const STABLE_FRAGMENT: i16 = 1 << 6;
    /// Indicates a fragment with keyed or partially keyed children
    const KEYED_FRAGMENT: i16 = 1 << 7;
    /// Indicates a fragment with unkeyed children.
    const UNKEYED_FRAGMENT: i16 = 1 << 8;
    /// Indicates an element that only needs non-props patching, e.g. ref or
    /// directives (onVnodeXXX hooks). since every patched vnode checks for refs
    /// and onVnodeXXX hooks, it simply marks the vnode so that a parent block
    /// will track it.
    const NEED_PATCH: i16 = 1 << 9;
    /// Indicates a component with dynamic slots (e.g. slot that references a v-for
    /// iterated value, or dynamic slot names).
    /// Components with this flag are always force updated.
    const DYNAMIC_SLOTS: i16 = 1 << 10;

    /// Indicates a fragment that was created only because the user has placed
    /// comments at the root level of a template. This is a dev-only flag since
    /// comments are stripped in production.
    const DEV_ROOT_FRAGMENT: i16 = 1 << 11;

    /// SPECIAL FLAGS -------------------------------------------------------------
    /// Special flags are negative integers. They are never matched against using
    /// bitwise operators (bitwise matching should only happen in branches where
    /// patchFlag > 0), and are mutually exclusive. When checking for a special
    /// flag, simply check patchFlag === FLAG.

    /// Indicates a hoisted static vnode. This is a hint for hydration to skip
    /// the entire sub tree since static content never needs to be updated.
    const HOISTED: i16 = -1;
    /// A special flag that indicates that the diffing algorithm should bail out
    /// of optimized mode. For example, on block fragments created by renderSlot()
    /// when encountering non-compiler generated slots (i.e. manually written
    /// render functions, which should always be fully diffed)
    /// OR manually cloneVNodes
    const BAIL: i16 = -2;
}
