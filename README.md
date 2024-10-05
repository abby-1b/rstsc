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
- [x] Type declaration removal
	- [x] `type A = 123`
	- [x] `type A<T> = Other<T>`
- [x] Type/Generic arguments (eg `Record<string, number>`)
	- [x] Test
- [x] Typed dictionaries
- [x] Interface removal
- [x] Class method/variable differentiation
- [x] Visibility modifiers in class constructors (auto-declaration, eg `constructor(public a: string)` -> `constructor(a) { this.a = a; }`)
- [ ] Enum transformation
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
- [ ] Switch statements
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
	- [ ] Mark all module imports as unused
	- [ ] As they're used (in code, not types), mark them as such
	- [ ] Only emit used imports
- [x] Automatic semicolon insertion (ASI)
	- [x] Basic support
	- [x] Context-based
- [x] Adding tests
	- [x] Test base! (directory, helper functions)
	- [x] Partial coverage
	- [ ] Full coverage
- [ ] Source maps
- [x] Building an AST
