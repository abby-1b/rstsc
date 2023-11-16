# RSTSC (Rust TypeScript Compiler)

A TypeScript compiler written in Rust!

This compiler focuses on speed over compatibility. It isn't meant to do type checking, and only compiles `.ts` files.

Due to its lack of a type system, const enums are not supported, with no future plans for adding them in.

# Roadmap
- [x] Visibility modifier removal
- [x] Function argument type-hint removal
- [x] Type declaration removal
- [x] Type arguments (eg `Record<string, number>`)
- [x] Typed dictionaries
- [x] Interface removal
- [x] Class method/variable differentiation
- [x] Enum transformation
- [ ] String literal support (using them causes errors for the moment!)
- [ ] Correctly parsing explicit generics (eg `fnCall<Type>(arg1)`)
- [ ] Adding tests
- [ ] Source maps
- [ ] Building an AST
