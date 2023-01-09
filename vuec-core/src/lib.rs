#![feature(fn_traits)]
#![feature(let_chains)]
#![feature(drain_filter)]
#![feature(is_some_and)]
use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

pub mod ast;
pub mod compat;
pub mod errors;
pub mod options;
pub mod parse;
pub mod re;
pub mod runtime_helpers;
pub mod transform;
pub mod transforms;
