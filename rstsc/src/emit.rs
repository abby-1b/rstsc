use crate::{ast::{ASTNode, ClassMember, FunctionDefinition, ObjectProperty}, ast_common::DestructurePattern, declaration::{Declaration, DeclarationComputable, DestructurableDeclaration}, rest::Rest, small_vec::SmallVec};

static NO_REST: Rest = Rest::new();

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
  pub fn out(&mut self, out: &str, can_put_semicolon: bool) {
    self.out_diff(out, out, can_put_semicolon);
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

  pub fn emit_vec<I: std::fmt::Debug, F>(
    &mut self,
    vector: &[I],
    mut traversal_fn: F,
    normal_separator: &str,
    compact_separator: &str
  ) where F: FnMut(&I, &mut Emitter) {
    let separator = if self.is_compact {
      compact_separator
    } else {
      normal_separator
    };
    let mut remaining = vector.len();
    for part in vector {
      traversal_fn(part, self);
      remaining -= 1;
      if remaining != 0 {
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
  if let ASTNode::Block { nodes } = ast {
    for node in nodes.iter() {
      emit_single(node, &mut emitter);
      emitter.endline();
    }
  } else {
    panic!("Expected `Block` node in `emit`!");
  }

  emitter.finalize()
}

fn emit_single(
  ast: &ASTNode,
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
    ASTNode::VariableDeclaration {
      modifiers,
      def_type,
      defs
    } => {
      emitter.out(&modifiers.emit(true), false);
      emitter.out(&def_type.emit(), false);
      emit_destructurable_declarations(defs.as_ref(), NO_REST.clone(), emitter);
      emitter.out("", true);
    }
    ASTNode::StatementImport { inner } => {
      emitter.out("import ", false);
      let mut parts =
        inner.default_alias.is_some() as i8 +
        inner.wildcard.is_some() as i8 +
        (inner.individual.len() > 0) as i8;
      let has_distinction = parts > 0;
      if let Some(default_alias) = &inner.default_alias {
        emitter.out(default_alias, false);
        if parts > 1 {
          emitter.out_diff(", ", ",", false);
          parts -= 1;
        }
      }

      if let Some(wildcard) = &inner.wildcard {
        emitter.out("* as ", false);
        emitter.out(&wildcard, false);
        if parts > 1 {
          emitter.out_diff(", ", ",", false);
          // parts -= 1;
        }
      }

      if inner.individual.len() > 0 {
        emitter.out_diff("{ ", "{", false);
        emitter.emit_vec(inner.individual.as_ref(), |i, emitter| {
          emitter.out(&i.name, false);
          if let Some(alias) = &i.alias {
            emitter.out(" as ", false);
            emitter.out(&alias, false);
          }
        }, ", ", ",");
        emitter.out_diff(" }", "}", false);
      }

      if has_distinction {
        emitter.out(" from ", false);
      }
      emitter.out(&inner.source, true);
    }
    ASTNode::ExpressionImport { value } => {
      emitter.out("import(", false);
      emit_single(value, emitter);
      emitter.out(")", true);
    }
    ASTNode::StatementIf { condition, body, alternate } => {
      let start_line = emitter.curr_line();

      // Head
      emitter.out("if (", false);
      emit_single(condition, emitter);
      emitter.out_diff(") ", ")", false);

      // Body
      emit_single(body, emitter);

      // Else (if any)
      if let Some(alternate) = alternate {
        if emitter.curr_line() == start_line {
          emitter.endline();
          emitter.out("else ", false);
        } else {
          emitter.out(" else ", false);
        }
        emit_single(alternate, emitter);
      }
    }
    // ASTNode::StatementWhile { .. } => "StatementWhile"
    ASTNode::StatementFor { init, condition, update, body } => {
      // Head
      emitter.out_diff("for (", "for(", false);
      emit_single(init, emitter);
      emitter.out_diff("; ", ";", false);
      emit_single(condition, emitter);
      emitter.out_diff("; ", ";", false);
      emit_single(update, emitter);
      emitter.out_diff(") ", ")", false);

      // Body
      emit_single(body, emitter);
    }
    ASTNode::StatementForOf { init, expression, body } => {
      // Head
      emitter.out_diff("for (", "for(", false);
      emit_single(init, emitter);
      emitter.out_diff(" of ", "of", false);
      emit_single(expression, emitter);
      emitter.out_diff(") ", ")", false);

      // Body
      emit_single(body, emitter);
    }
    ASTNode::StatementForIn { init, expression, body } => {
      // Head
      emitter.out_diff("for (", "for(", false);
      emit_single(init, emitter);
      emitter.out_diff(" in ", "in", false);
      emit_single(expression, emitter);
      emitter.out_diff(") ", ")", false);

      // Body
      emit_single(body, emitter);
    }
    ASTNode::StatementSwitch { condition, cases, default } => {
      emitter.out("switch (", false);
      emit_single(condition, emitter);
      emitter.out(") {", false);
      emitter.endline();
      emitter.indent();

      for (case_cond, case_body) in cases.iter() {
        emitter.out("case ", false);
        emit_single(case_cond, emitter);
        emitter.out(": ", false);
        emitter.endline();
        emitter.indent();
        for stmt in case_body.iter() {
          emit_single(stmt, emitter);
          emitter.endline();
        }
        emitter.unindent();
      }

      if let Some(default_body) = default {
        emitter.out("default: ", false);
        emitter.endline();
        emitter.indent();
        for stmt in default_body.iter() {
          emit_single(stmt, emitter);
          emitter.endline();
        }
        emitter.unindent();
      }

      emitter.unindent();
      emitter.out("}", false);
    }
    ASTNode::StatementReturn { value } => {
      emitter.out("return", true);
      if let Some(value) = value {
        emitter.out(" ", false);
        emit_single(value, emitter);
      }
    }
    ASTNode::StatementBreak { value } => {
      emitter.out("break", true);
      if let Some(value) = value {
        emitter.out(" ", false);
        emit_single(value, emitter);
      }
    }
    ASTNode::StatementContinue { value } => {
      emitter.out("continue", true);
      if let Some(value) = value {
        emitter.out(" ", false);
        emit_single(value, emitter);
      }
    }
    ASTNode::StatementThrow { value } => {
      emitter.out("throw", true);
      if let Some(value) = value {
        emitter.out(" ", false);
        emit_single(value, emitter);
      }
    }
    ASTNode::FunctionDefinition { inner } => {
      if inner.body.is_some() {
        emit_function_definition(
          &*inner,
          "function",
          emitter
        );
      }
    }
    ASTNode::ArrowFunctionDefinition { inner: arrow_fn } => {
      // Params
      if arrow_fn.params.len() == 1 && arrow_fn.params[0].value().is_none() {
        emitter.out(&arrow_fn.params[0].name(), false);
      } else {
        emitter.out("(", false);
        emit_declarations(arrow_fn.params.as_ref(), arrow_fn.rest.clone(), emitter);
        emitter.out(")", false);
      }

      // Arrow
      emitter.out_diff(" => ", "=>", false);

      // Body
      emit_single(&arrow_fn.body, emitter);
    }
    ASTNode::ClassDefinition { inner } => {
      emitter.out(&inner.modifiers.emit(true), false);
      if let Some(name) = &inner.name {
        emitter.out("class ", false);
        emitter.out(&name, false);
      } else {
        emitter.out("class", false);
      }
      if let Some(extends) = &inner.extends {
        emitter.out(" extends ", false);
        emitter.out(&extends.get_single_name(), false);
      }
      emitter.out_diff(" {", "{", false);
      emitter.endline();
      emitter.indent();

      for member in inner.members.iter() {
        match member {
          ClassMember::Property(declaration, modifiers) => {
            emitter.out(&modifiers.emit(true), false);
            emit_single_declaration_computable(declaration, emitter);
            emitter.endline();
          }
          ClassMember::Method(method, getter_setter) => {
            if method.body.is_none() { continue; }
            emit_function_definition(
              &method,
              getter_setter.as_str(),
              emitter
            );
            emitter.endline();
          }
          ClassMember::StaticBlock(body) => {
            emitter.out("static ", false);
            emit_single(body, emitter);
            emitter.endline();
          }
        }
      }

      emitter.endline();
      emitter.unindent();
      emitter.out("}", false)
    }
    // ASTNode::PotentialParameter { .. } => "PotentialParameter",
    // ASTNode::ArrowFunctionHeader { .. } => "ArrowFunctionHeader",
    ASTNode::Parenthesis { nodes } => {
      emitter.out("(", false);
      emitter.emit_vec(nodes.as_ref(), |node, emitter| {
        emit_single(&node, emitter);
      }, ", ", ",");
      emitter.out(")", true);
    }
    ASTNode::Array { nodes } => {
      emitter.out_diff("[ ", "[", false);
      emitter.emit_vec(nodes.as_ref(), |node, emitter| {
        emit_single(&node, emitter);
      }, ", ", ",");
      emitter.out_diff(" ]", "]", true);
    }
    ASTNode::Dict { properties } => {
      emitter.out_diff("{ ", "{", false);
      emitter.emit_vec(properties.as_ref(), |prop, emitter| {
        match prop {
          ObjectProperty::Rest { argument } => {
            emitter.out("...", false);
            emit_single(&argument, emitter);
          }
          ObjectProperty::Property { computed, key, value } => {
            if *computed { emitter.out("[", false); }
            emit_single(key, emitter);
            if *computed { emitter.out("]", false); }
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
    ASTNode::ExprTemplateLiteral { head, parts } => {
      emitter.out("`", false);
      emitter.out(head, false);
      for (expr, literal_part) in parts {
        emitter.out("${", false);
        emit_single(expr, emitter);
        emitter.out("}", false);
        emitter.out(literal_part, false);
      }
      emitter.out("`", true);
    }
    ASTNode::ExprIdentifier { name } => {
      emitter.out(&name, true);
    }
    ASTNode::ExprBoolLiteral { value } => {
      emitter.out(if *value { "true" } else { "false" }, true);
    }
    ASTNode::ExprFunctionCall { callee, generics: _, arguments } => {
      emit_single(&*callee, emitter);
      emitter.out("(", false);
      emitter.emit_vec(arguments.as_ref(), |argument, emitter| {
        emit_single(argument, emitter);
      }, ", ", ",");
      emitter.out(")", true);
    }
    ASTNode::ExprIndexing { callee, property } => {
      emit_single(&*callee, emitter);
      emitter.out("[", false);
      emit_single(&*property, emitter);
      emitter.out("]", true);
    }
    ASTNode::ExprTernary { condition, if_true, if_false } => {
      emit_single(&*condition, emitter);
      emitter.out_diff(" ? ", "?", false);
      emit_single(&*if_true, emitter);
      emitter.out_diff(" : ", ":", false);
      emit_single(&*if_false, emitter);
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

      emit_single(&*expr, emitter);
    }
    ASTNode::InfixOpr { left, opr, right } => {
      let can_have_spaces = match opr.as_str() {
        "." => (false, false),
        "?." => (false, false),
        "," => (false, true),
        _ => (true, true)
      };
      emit_single(&*left, emitter);
      if can_have_spaces.0 { emitter.out_diff(" ", "", false); }
      emitter.out(&opr, false);
      if can_have_spaces.1 { emitter.out_diff(" ", "", false); }
      emit_single(&*right, emitter);
    }
    ASTNode::PostfixOpr { expr, opr } => {
      emit_single(&*expr, emitter);
      emitter.out(&opr, true);
    }
    ASTNode::NonNullAssertion { expr } => {
      emit_single(&*expr, emitter);
    }
    ASTNode::ExprAs { value, .. } => { emit_single(&*value, emitter); }
    ASTNode::ExprTypeAssertion { value, .. } => { emit_single(&*value, emitter); }
    ASTNode::EnumDeclaration { inner } => {
      emitter.out("var ", false);
      emitter.out(&inner.name, true);
      emitter.endline();

      emitter.out("(function(", false);
      emitter.out(&inner.name, false);
      emitter.out("){", false);
      emitter.endline();
      emitter.indent();

      for (name, value) in &inner.members {
        emitter.out(&inner.name, false);
        emitter.out("[", false);
        emitter.out(&inner.name, false);
        emitter.out("[", false);
        emitter.out(name, false);
        emitter.out("]=", false);
        emit_single(value, emitter);
        emitter.out("]=", false);
        emitter.out(name, true);
        emitter.endline();
      }

      emitter.endline();
      emitter.unindent();
      emitter.out("})(", false);
      emitter.out(&inner.name, true);
      emitter.out("||(", false);
      emitter.out(&inner.name, true);
      emitter.out("={}))", true);
    },
    ASTNode::InterfaceDeclaration { .. } => {},
    ASTNode::Empty { .. } => {}
    other => {
      emitter.out(&format!("[ {} ]", other.name()), true);
    }
  }
}

fn emit_declarations(
  declarations: &[Declaration],
  rest: Rest,
  emitter: &mut Emitter
) {
  let mut i = rest.index_in_decls(declarations.len());
  emitter.emit_vec(declarations, |decl, emitter| {
    i -= 1;
    if i == 0 { emitter.out("...", false); }
    emit_single_declaration(decl, emitter);
  }, ", ", ",");
}

fn emit_destructurable_declarations(
  declarations: &[DestructurableDeclaration],
  rest: Rest,
  emitter: &mut Emitter
) {
  let mut i = rest.index_in_decls(declarations.len());
  emitter.emit_vec(declarations, |declaration, emitter| {
    i -= 1;
    if i == 0 { emitter.out("...", false); }
    emit_single_variable_declaration(declaration, emitter);
  }, ", ", ",");
}

fn emit_single_declaration_computable(
  declaration: &DeclarationComputable,
  emitter: &mut Emitter
) {
  if declaration.name.is_named() {
    emitter.out(
      unsafe { &declaration.name.get_named_unchecked() },
      true
    );
  } else {
    emitter.out("[", false);
    emit_single(
      unsafe { &declaration.name.get_computed_unchecked() },
      emitter
    );
    emitter.out("]", false);
  }
  if let Some(value) = &declaration.value {
    emitter.out_diff(" = ", "=", false);
    emit_single(value, emitter);
  }
}

fn emit_single_declaration(
  declaration: &Declaration,
  emitter: &mut Emitter
) {
  emitter.out(&declaration.name(), true);
  if let Some(value) = declaration.value() {
    emitter.out_diff(" = ", "=", false);
    emit_single(value, emitter);
  }
}

fn emit_single_variable_declaration(
  declaration: &DestructurableDeclaration,
  emitter: &mut Emitter
) {
  emit_destructure_pattern(&declaration.name, emitter);
  if let Some(value) = &declaration.initializer {
    emitter.out_diff(" = ", "=", false);
    emit_single(value, emitter);
  }
}

fn emit_destructure_pattern(
  pattern: &DestructurePattern,
  emitter: &mut Emitter
) {
  match pattern {
    DestructurePattern::Array { elements, spread } => {
      emitter.out_diff("[ ", "[", false);
      emitter.emit_vec(elements.as_ref(), |element, emitter| {
        emit_destructure_pattern(element, emitter);
      }, ", ", ",");
      if let Some(rest) = spread {
        if elements.len() > 0 {
          emitter.out_diff(", ", ",", false);
        }
        emitter.out("...", false);
        emit_destructure_pattern(rest, emitter);
      }
      emitter.out_diff(" ]", "]", true);
    }
    DestructurePattern::Object { properties, spread } => {
      emitter.out_diff("{ ", "{", false);
      emitter.emit_vec(properties.as_ref(), |(name, pattern), emitter| {
        emitter.out(name, false);
        emitter.out_diff(": ", ":", false);
        emit_destructure_pattern(pattern, emitter);
      }, ", ", ",");
      if let Some(spread) = spread {
        if properties.len() > 0 {
          emitter.out_diff(", ", ",", false);
        }
        emitter.out("...", false);
        emit_destructure_pattern(spread, emitter);
      }
      emitter.out_diff(" }", "}", true);
    }
    DestructurePattern::Identifier { name } => {
      emitter.out(name, true);
    }
    DestructurePattern::Ignore => {
      emitter.out("", true);
    }
  }
}

fn emit_function_definition(
  function: &FunctionDefinition,
  before_name_statement: &str,
  emitter: &mut Emitter
) {
  // Head
  emitter.out(&function.modifiers.emit(true), false);
  if before_name_statement.len() > 0 {
    emitter.out(before_name_statement, false);
    emitter.out(" ", false);
  }
  if let Some(name) = &function.name {
    emitter.out(&name, false);
  }

  // Params
  emitter.out("(", false);
  let fake_this = if function.params.len() > 0 {
    if let DestructurePattern::Identifier { name } = &function.params[0].name {
      name == "this"
    } else {
      false
    }
  } else { false };
  emit_destructurable_declarations(
    if fake_this {
      &function.params[1..]
    } else {
      function.params.as_ref()
    },
    function.rest.clone(),
    emitter
  );
  emitter.out_diff(") ", ")", false);

  // Body
  if let Some(body) = &function.body {
    emit_single(&body, emitter);
  }
}
