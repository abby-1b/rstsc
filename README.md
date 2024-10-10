![Banner](./assets/banner.svg)

# RSTSC: Rust TypeScript Compiler

RSTSC is a TypeScript compiler written entirely in Rust. It provides a fast and efficient way to compile TypeScript code.

The focus on memory efficiency makes RSTSC well-suited for projects that deal with large TypeScript codebases.

(Also, it's pronounced "rustic"!)

# Roadmap

### Phase 1: Core Syntax Parsing (80% Completed)

Completed Tasks:
 - Building an Abstract Syntax Tree (AST)
 - Visibility modifier removal (public, private, protected)
 - Function type-hint removal
 - Type declaration removal
 - Basic type/generic arguments handling (e.g., `Record<string, number>`)
 - Typed dictionaries
 - Interface removal
 - Class method/variable differentiation
 - Visibility modifiers in class constructors (auto-declaration)
 - Arrow functions (code and types)
 - Ternary operations
 - Conditional chaining
 - Non-null assertion removal
 - Automatic semicolon insertion (ASI, basic and context-based)

Remaining Tasks:
 - Enum transformation
 - String literal support (types inside literals)
 - Switch statements
 - Full non-null assertion handling

### Phase 2: Advanced Syntax and Declarations

Tasks:
 - Declarations
 - Variable scope tracking
 - Module import removal (unused import handling and emission)
 - Source map generation

### Phase 3: Test Coverage & Efficiency

Tasks:
 - Complete test coverage for everything
 - Optimize compiler performance for speed and resource efficiency



# License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.
