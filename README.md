# RSTSC (Rust TypeScript Compiler)

A TypeScript compiler written in Rust!

This compiler focuses on speed over compatibility. It isn't meant to do type checking, and only compiles `.ts` files.

Due to its lack of a type system, const enums are not supported, with no future plans for adding them in.

# Roadmap
- [x] Visibility modifier removal
	- [x] Tests
- [x] Function argument type-hint removal
	- [x] Test
- [x] Type declaration removal
	- [x] Test
- [x] Type/Generic arguments (eg `Record<string, number>`)
	- [x] Test
- [x] Typed dictionaries
- [x] Interface removal
- [x] Class method/variable differentiation
- [ ] Visibility modifiers in class constructors (auto-declaration, eg `constructor(public a: string)` -> `constructor(a) { this.a = a; }`)
- [x] Enum transformation
- [ ] String literal support
	- [x] Basic support (no compiling inside literals)
	- [ ] Types inside of string literals
- [x] Correctly parsing explicit generics (eg `fnCall<Type>(arg1)`)
- [x] Arrow functions
	- [x] In code
	- [x] Types
- [x] Ternaries (eg `condition ? value_if_true : value_if_false`)
- [x] Conditional chaining (eg `potentially_undefined_val?.some_method(...)`)
- [x] Non-null assertion removal
	- [x] When the last token is a name, number, or string
	- [x] When the last token is a closing bracket
- [x] Switch statements (fix due to tagging of case body as type)
- [x] Declarations
	- [x] `declare namespace ... { ... }`
		- [x] `declare namespace some_name { ... }`
		- [x] `declare namespace some_name.foo { ... }`
		- [x] `declare namespace some_name.const { ... }` (for some reason `const` works as a property type?)
	- [x] `declare global { ... }`
	- [x] `declare class { ... }`
	- [x] `declare function ...`
	- [x] `declare const/let/var ...`
- [ ] Variable scope (necessary for correct module import removal)
	- [ ] When a variable is declared, add it to the scope
		- [ ] Maybe also add its type?
	- [ ] When the scope is over, delete the variables in this level from scope
- [ ] Module import removal (eg `import { Type } from '...'; const a: Type;`)
	- [ ] Mark all module imports for removal
	- [ ] Un-mark them (set them to 0) when they're used
- [ ] Automatic semicolon insertion (ASI)
	- [x] VERY basic support (and a slightly general base)
	- [ ] Context-based
- [ ] Adding tests
	- [x] Test base! (directory, helper functions)
	- [x] Partial coverage
	- [ ] Full coverage
- [ ] Source maps
- [ ] Building an AST
	- [ ] Hi! Sorry, uhh I just saw this and ***WHAT???***
	- [ ] Building an AST from source basically (pretty much (in short)) defeats the WHOLE PURPOSE of 80% of the work that's already done.
	- [ ] It's basically a from-scratch re-write of the WHOLE project.
	- [ ] If there is, for some ungodly reason, any movement towards making an AST, please remember to make a separate branch in this repository.
	- [ ] Good luck.
