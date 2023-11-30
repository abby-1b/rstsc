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
- [ ] String literal support
	- [x] Basic support
	- [ ] Types inside of string literals
- [x] Correctly parsing explicit generics (eg `fnCall<Type>(arg1)`)
- [x] Arrow functions
- [x] Ternaries (eg `condition ? value_if_true : value_if_false`)
- [x] Conditional chaining (eg `potentially_undefined_val?.some_method(...)`)
- [x] Non-null assertion removal (currently they're left in)
- [ ] Automatic semicolon insertion (ASI)
	- [x] VERY basic support (and a slightly general base)
	- [ ] Context-based
- [ ] Adding tests
- [ ] Source maps
- [ ] Building an AST
