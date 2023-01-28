#![feature(fn_traits)]
#![feature(let_chains)]
#![feature(drain_filter)]
#![feature(is_some_and)]

// TODO: Need to Cond Compile
// Global compile-time constants
pub static __DEV__: bool = true;
pub static __TEST__: bool = false;
pub static __BROWSER__: bool = false;
pub static __GLOBAL__: bool = false;
pub static __ESM_BUNDLER__: bool = false;
pub static __ESM_BROWSER__: bool = false;
pub static __NODE_JS__: bool = false;
pub static __SSR__: bool = false;
pub static __COMMIT__: &str = "";
pub static __VERSION__: &str = "";
pub static __COMPAT__: bool = false;

// Feature flags
pub static __FEATURE_OPTIONS_API__: bool = false;
pub static __FEATURE_PROD_DEVTOOLS__: bool = false;
pub static __FEATURE_SUSPENSE__: bool = false;

use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

pub mod ast;
pub mod codegen;
pub mod compat;
pub mod errors;
pub mod options;
pub mod parse;
pub mod re;
pub mod runtime_helpers;
pub mod shared;
pub mod transform;
pub mod transforms;
pub mod utils;
