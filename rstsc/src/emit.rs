use crate::ast::{ASTNode, FunctionDefinition, NamedDeclaration, ObjectProperty};

struct Emitter {
    output: String,
    is_compact: bool,
    line_has_data: bool,
    is_mid_line: bool,
    indent_level: usize,
    curr_line: usize,
}

impl Emitter {
    pub fn curr_line(&self) -> usize {
        self.curr_line
    }

    /// Output the same string, no matter if the mode is compact or not
    pub fn out(&mut self, same: &str, can_put_semicolon: bool) {
        self.out_diff(same, same, can_put_semicolon);
    }

    /// Outputs different strings depending on if compact mode is on or off
    pub fn out_diff(&mut self, normal: &str, compact: &str, can_put_semicolon: bool) {
        #[cfg(debug_assertions)]
        {
            for c in normal.chars().chain(compact.chars()) {
                if c == '\n' {
                    panic!("Newline found in `emit.out(...)`, please use `emitter.endline();`");
                }
            }
        }

        if self.is_compact {
            self.output += compact;
        } else {
            if !self.line_has_data {
                self.output += &"  ".repeat(self.indent_level);
            }
            self.output += normal;
        }
        self.line_has_data = true;
        self.is_mid_line = can_put_semicolon;
    }

    pub fn emit_vec<I, F>(
        &mut self,
        vector: Vec<I>,
        traversal_fn: F,
        normal_separator: &str,
        compact_separator: &str
    ) where F: Fn(I, &mut Emitter) {
        if vector.is_empty() { return; }
        let separator = if self.is_compact {
            compact_separator
        } else {
            normal_separator
        };
        let last_index = vector.len() - 1;
        for (index, argument) in vector.into_iter().enumerate() {
            traversal_fn(argument, self);
            if index != last_index {
                self.out(separator, false);
            }
        }
    }

    pub fn indent(&mut self) {
        self.indent_level += 1;
    }
    pub fn unindent(&mut self) {
        self.indent_level -= 1;
    }

    /// Ends a line!
    pub fn endline(&mut self) {
        if self.is_mid_line {
            self.output += ";";
            self.is_mid_line = false;
        }
        if self.line_has_data {
            if !self.is_compact { self.output += "\n"; }
            self.line_has_data = false;
            self.curr_line += 1;
        }
    }

    /// Outputs, destroying the emitter
    pub fn finalize(self) -> String {
        self.output
    }
}

/// Emits code given an AST
pub fn emit_code(ast: ASTNode, compact: bool) -> String {
    let mut emitter = Emitter {
        output: String::new(),
        is_compact: compact,
        line_has_data: false,
        is_mid_line: false,
        indent_level: 0,
        curr_line: 0
    };

    // Handle the program `Block` node
    match ast {
        ASTNode::Block { nodes } => {
            for node in nodes {
                emit_single(node, &mut emitter);
                emitter.endline();
            }
        }
        _ => { panic!("Expected `Block` node in `emit`!"); }
    }

    emitter.finalize()
}

fn emit_single(
    ast: ASTNode,
    emitter: &mut Emitter
) {
    match ast {
        ASTNode::Block { nodes } => {
            emitter.out("{", false);
            emitter.endline();
            emitter.indent();
            for node in nodes {
                emit_single(node, emitter);
                emitter.endline();
            }
            emitter.endline();
            emitter.unindent();
            emitter.out("}", false);
        }
        ASTNode::Declaration { on, .. } => {
            emit_single(*on, emitter);
        }
        ASTNode::VariableDeclaration { modifiers, def_type, defs } => {
            emitter.out(&modifiers.emit(true), false);
            emitter.out(&def_type.emit(), false);
            emit_named_declarations(defs, emitter);
            emitter.out("", true);
        }
        ASTNode::StatementIf { condition, body, alternate } => {
            let start_line = emitter.curr_line();

            // Head
            emitter.out("if (", false);
            emit_single(*condition, emitter);
            emitter.out_diff(") ", ")", false);

            // Body
            emit_single(*body, emitter);

            // Else (if any)
            if let Some(alternate) = alternate {
                if emitter.curr_line() == start_line {
                    emitter.endline();
                    emitter.out("else ", false);
                } else {
                    emitter.out(" else ", false);
                }
                emit_single(*alternate, emitter);
            }
        }
        // ASTNode::StatementWhile { .. } => "StatementWhile"
        ASTNode::StatementFor { init, condition, update, body } => {
            // Head
            emitter.out_diff("for (", "for(", false);
            emit_single(*init, emitter);
            emitter.out_diff("; ", ";", false);
            emit_single(*condition, emitter);
            emitter.out_diff("; ", ";", false);
            emit_single(*update, emitter);
            emitter.out_diff(") ", ")", false);

            // Body
            emit_single(*body, emitter);
        }
        ASTNode::StatementReturn { value } => {
            emitter.out("return", true);
            if let Some(value) = value {
                emitter.out(" ", false);
                emit_single(*value, emitter);
            }
        }
        ASTNode::StatementBreak { value } => {
            emitter.out("break", true);
            if let Some(value) = value {
                emitter.out(" ", false);
                emit_single(*value, emitter);
            }
        }
        ASTNode::StatementContinue { value } => {
            emitter.out("continue", true);
            if let Some(value) = value {
                emitter.out(" ", false);
                emit_single(*value, emitter);
            }
        }
        ASTNode::StatementThrow { value } => {
            emitter.out("throw", true);
            if let Some(value) = value {
                emitter.out(" ", false);
                emit_single(*value, emitter);
            }
        }
        ASTNode::FunctionDefinition { inner } => {
            if inner.body.is_some() {
                emit_function_definition(
                    *inner,
                    true,
                    emitter
                );
            }
        }
        ASTNode::ArrowFunctionDefinition { params, body, .. } => {
            // Params
            if params.len() == 1 && params[0].value.is_none() {
                emitter.out(&params[0].name, false);
            } else {
                emitter.out("(", false);
                emit_named_declarations(params, emitter);
                emitter.out(")", false);
            }

            // Arrow
            emitter.out_diff(" => ", "=>", false);

            // Body
            emit_single(*body, emitter);
        }
        ASTNode::ClassDefinition { inner } => {
            emitter.out(&inner.modifiers.emit(true), false);
            emitter.out("class ", false);
            emitter.out(&inner.name, false);
            if let Some(extends) = inner.extends {
                emitter.out(" extends ", false);
                emitter.out(&extends.get_single_name(), false);
            }
            emitter.out_diff(" {", "{", false);
            emitter.endline();
            emitter.indent();

            for (decl_modifiers, declaration) in inner.declarations {
                emitter.out(&decl_modifiers.emit(true), false);
                emit_named_declaration(declaration, emitter);
                emitter.endline();
            }
            for method in inner.methods {
                if method.body.is_none() { continue; }
                emit_function_definition(method, false, emitter);
                emitter.endline();
            }

            emitter.endline();
            emitter.unindent();
            emitter.out("}", false)
        }
        // ASTNode::PotentialParameter { .. } => "PotentialParameter",
        // ASTNode::ArrowFunctionHeader { .. } => "ArrowFunctionHeader",
        ASTNode::Parenthesis { nodes } => {
            emitter.out("(", false);
            emitter.emit_vec(nodes, |node, emitter| {
                emit_single(node, emitter);
            }, ", ", ",");
            emitter.out(")", true);
        }
        ASTNode::Array { nodes } => {
            emitter.out_diff("[ ", "[", false);
            emitter.emit_vec(nodes, |node, emitter| {
                emit_single(node, emitter);
            }, ", ", ",");
            emitter.out_diff(" ]", "]", true);
        }
        ASTNode::Dict { properties } => {
            emitter.out_diff("{ ", "{", false);
            emitter.emit_vec(properties, |prop, emitter| {
                match prop {
                    ObjectProperty::Spread { argument } => {
                        emitter.out("...", false);
                        emit_single(argument, emitter);
                    }
                    ObjectProperty::Property { computed, key, value } => {
                        if computed { emitter.out("[", false); }
                        emit_single(key, emitter);
                        if computed { emitter.out("]", false); }
                        emitter.out_diff(": ", ":", false);
                        emit_single(value, emitter);
                    }
                }
            }, ", ", ",");
            emitter.out_diff(" }", "}", true);
        }
        ASTNode::ExprNumLiteral { number } => {
            emitter.out(&number.to_string(), true);
        }
        ASTNode::ExprStrLiteral { string } => {
            emitter.out(&string, true);
        }
        ASTNode::ExprIdentifier { name } => {
            emitter.out(&name, true);
        }
        ASTNode::ExprBoolLiteral { value } => {
            emitter.out(if value { "true" } else { "false" }, true);
        }
        ASTNode::ExprFunctionCall { callee, arguments } => {
            emit_single(*callee, emitter);
            emitter.out("(", false);
            emitter.emit_vec(arguments, |argument, emitter| {
                emit_single(argument, emitter);
            }, ", ", ",");
            emitter.out(")", true);
        }
        ASTNode::ExprIndexing { callee, property } => {
            emit_single(*callee, emitter);
            emitter.out("(", false);
            emit_single(*property, emitter);
            emitter.out(")", true);
        }
        ASTNode::ExprTernary { condition, if_true, if_false } => {
            emit_single(*condition, emitter);
            emitter.out_diff(" ? ", "?", false);
            emit_single(*if_true, emitter);
            emitter.out_diff(" : ", ":", false);
            emit_single(*if_false, emitter);
        }
        ASTNode::PrefixOpr { opr, expr } => {
            emitter.out(&opr, true);

            // Identifier operators (like `typeof` and `yield`) need a space!
            let first_char = unsafe {
                *((&opr[0..1] as *const str) as *const u8)
            } as char;
            if first_char.is_ascii_alphabetic() {
                emitter.out(" ", false);
            }

            emit_single(*expr, emitter);
        }
        ASTNode::InfixOpr { left, opr, right } => {
            let can_have_spaces = match opr.as_str() {
                "." => (false, false),
                "?." => (false, false),
                "," => (false, true),
                _ => (true, true)
            };
            emit_single(*left, emitter);
            if can_have_spaces.0 { emitter.out_diff(" ", "", false); }
            emitter.out(&opr, false);
            if can_have_spaces.1 { emitter.out_diff(" ", "", false); }
            emit_single(*right, emitter);
        }
        ASTNode::PostfixOpr { expr, opr } => {
            emit_single(*expr, emitter);
            emitter.out(&opr, true);
        }
        ASTNode::ExprAs { value, .. } => { emit_single(*value, emitter); }
        ASTNode::ExprTypeAssertion { value, .. } => { emit_single(*value, emitter); }
        ASTNode::TypeDeclaration { .. } => {}
        ASTNode::Empty { .. } => {}
        other => {
            emitter.out(&format!("[ {} ]", other.name()), true);
        }
    }
}

fn emit_named_declarations(
    decls: Vec<NamedDeclaration>,
    emitter: &mut Emitter
) {
    emitter.emit_vec(decls, |decl, emitter| {
        emit_named_declaration(decl, emitter);
    }, ", ", ",");
}

#[inline]
fn emit_named_declaration(
    declaration: NamedDeclaration,
    emitter: &mut Emitter
) {
    if declaration.spread {
        emitter.out("...", false);
    }
    emitter.out(&declaration.name, true);
    if let Some(value) = declaration.value {
        emitter.out_diff(" = ", "=", false);
        emit_single(value, emitter);
    }
}

fn emit_function_definition(
    function: FunctionDefinition,
    function_statement: bool,
    emitter: &mut Emitter
) {
    // Head
    emitter.out(&function.modifiers.emit(true), false);
    if function_statement {
        emitter.out("function ", false);
    }
    if let Some(name) = function.name {
        emitter.out(&name, false);
    }

    // Params
    emitter.out("(", false);
    emit_named_declarations(function.params, emitter);
    emitter.out_diff(") ", ")", false);

    // Body
    if let Some(body) = function.body {
        emit_single(*body, emitter);
    }
}
