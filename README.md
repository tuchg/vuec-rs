# Vuec-rs

A rust implementation of the `Vue.js` compiler that is faithful to the original and focused on synchronizing the
open-source community work at the lowest cost, while also providing high performance from rust.

> This project is still in the early stage of development, and the core part is not yet complete, and has some fundamental problems.
> 
> If you are interested in this project, you can join us and contribute to it.
>

## Design
* Try using pattern of `enum + struct` to impl ~OOP~ in rust
  * Deconstructing the tedious
  * Numerous conditional branches

## Roadmap

- [x] core
    - [x] parser
    - [ ] transforms
    - [ ] codegen
    - [ ] compiler
- [ ] dom
- [ ] sfc
- [ ] ssr
- [ ] plugins
