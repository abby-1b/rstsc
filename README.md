![Banner](./assets/banner.svg)

# RSTSC: Rust TypeScript Compiler

RSTSC is a TypeScript compiler written entirely in Rust. It provides a fast and efficient way to compile TypeScript code.

The focus on memory efficiency makes RSTSC well-suited for projects that deal with large TypeScript codebases.

(Also, it's pronounced "rustic"!)

# Performance

Currently, `rstsc` is able to fully parse files at **147k lines/second**.
These lines are only counted if they have at least one non-whitespace character.
Note that this is very early on, some features are missing from the compiler.

According to [this](https://github.com/microsoft/Typescript/wiki/Performance)
article by Microsoft, the `tsc` compiler can parse and emit 24906 lines in 1.12
seconds (excluding the time it takes to do type-checking, since `rstsc` doesn't
do that), meaning the compiler can do around **22k lines/second**, where they
count whitespace-only lines as lines of code (giving them an advantage).

Running a similar benchmark (with a 100k line file, no comments or
whitespace-only lines) gave me **37k lines/second** on my machine excluding type checking time.

# Roadmap

### Phase 1: Core Syntax Parsing (100% Completed)

Tasks:
- [x] Building an Abstract Syntax Tree (AST)
- [x] Visibility modifier removal (public, private, protected)
- [x] Function type-hint removal
- [x] Type declaration removal
- [x] Basic type/generic arguments handling (e.g., `Record<string, number>`)
- [x] Typed dictionaries
- [x] Interface removal
- [x] Class method/variable differentiation
- [x] Visibility modifiers in class constructors (auto-declaration)
- [x] Arrow functions (code and types)
- [x] Ternary operations
- [x] Conditional chaining
- [x] Non-null assertion removal
- [x] Basic Automatic Semicolon Insertion (ASI)
- [x] Enum transformation
- [x] Switch statements
- [x] Full non-null assertion handling
- [x] String literal support (types inside literals)
- [x] Basic regex parsing

### Phase 2: Advanced Syntax and Declarations

Tasks:
- [x] Add TSC project test snippets
- [x] Module import removal (unused import handling and emission)
- [ ] Variable scope tracking
- [ ] Declarations
- [ ] Source map generation
- [ ] Full ASI

### Phase 3: Test Coverage & Efficiency

Tasks:
- Complete test coverage for everything
- Optimize compiler performance for speed and resource efficiency
  - Make strings within the project reference the source file instead of allocating separately
  - When the file isn't loaded, make a string pool that the AST can reference
- Full regex parsing


# License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.
