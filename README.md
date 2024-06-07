# RSTSC (Rust TypeScript Compiler)

A TypeScript compiler written in Rust!

This compiler focuses on speed over compatibility. It isn't meant to do type checking, and only compiles `.ts` files.

Due to its lack of a type system, const enums are not supported, with no future plans for adding them in.

# Roadmap
- [x] Visibility modifier removal
	- [x] `public x: number = 123;`
	- [x] `private x: number = 123;`
	- [x] `protected x: number = 123;`
- [x] Function type-hint removal
	- [x] `function someFn(x): number { ... }`
	- [x] `function someFn(x: number) { ... }`
	- [x] `function someFn(x: number): number { ... }`
- [ ] Type declaration removal
	- [ ] `type A = 123`
	- [ ] `type A<T> = Other<T>`
- [ ] Type/Generic arguments (eg `Record<string, number>`)
	- [ ] Test
- [ ] Typed dictionaries
- [ ] Interface removal
- [ ] Class method/variable differentiation
- [ ] Visibility modifiers in class constructors (auto-declaration, eg `constructor(public a: string)` -> `constructor(a) { this.a = a; }`)
- [ ] Enum transformation
- [ ] String literal support
	- [ ] Basic support (no compiling inside literals)
	- [ ] Types inside of string literals
- [ ] Correctly parsing explicit generics (eg `fnCall<Type>(arg1)`)
- [ ] Arrow functions
	- [ ] In code
	- [ ] Types
- [ ] Ternaries (eg `condition ? value_if_true : value_if_false`)
- [ ] Conditional chaining (eg `potentially_undefined_val?.some_method(...)`)
- [ ] Non-null assertion removal
	- [ ] When the last token is a name, number, or string
	- [ ] When the last token is a closing bracket
- [ ] Switch statements (fix due to tagging of case body as type)
- [ ] Declarations
	- [ ] `declare namespace ... { ... }`
		- [ ] `declare namespace some_name { ... }`
		- [ ] `declare namespace some_name.foo { ... }`
		- [ ] `declare namespace some_name.const { ... }` (for some reason `const` works as a property type?)
	- [ ] `declare global { ... }`
	- [ ] `declare class { ... }`
	- [ ] `declare function ...`
	- [ ] `declare const/let/var ...`
- [ ] Variable scope (necessary for correct module import removal)
	- [ ] When a variable is declared, add it to the scope, along with its type.
	- [ ] When the scope is over, delete the variables in this level from scope
- [ ] Module import removal (eg `import { Type } from '...'; const a: Type;`)
	- [ ] Mark all module imports for removal
	- [ ] Un-mark them (set them to 0) when they're used
- [ ] Automatic semicolon insertion (ASI)
	- [ ] VERY basic support (and a slightly general base)
	- [ ] Context-based
- [ ] Adding tests
	- [ ] Test base! (directory, helper functions)
	- [ ] Partial coverage
	- [ ] Full coverage
- [ ] Source maps
- [x] Building an AST
