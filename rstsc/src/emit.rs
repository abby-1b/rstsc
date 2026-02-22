use crate::{
  ast::{ASTIndex, ASTNode, ClassMember, FunctionDefinition, ImportDefinition, ObjectProperty}, declaration::{DeclarationComputable, DestructurableDeclaration, DestructurePattern}, rest::Rest, source_properties::SourceProperties
};

static NO_REST: Rest = Rest::new();

struct Emitter<'a> {
  output: String,
  is_compact: bool,
  line_has_data: bool,
  is_mid_line: bool,
  indent_level: usize,
  curr_line: usize,
  sp: &'a SourceProperties,
}

impl<'a> Emitter<'a> {
  pub fn curr_line(&self) -> usize {
    self.curr_line
  }

  /// Output the same string, no matter if the mode is compact or not
  pub fn out(&mut self, out: &str, can_put_semicolon: bool) {
    self.out_diff(out, out, can_put_semicolon);
  }

  /// Outputs different strings depending on if compact mode is on or off
  pub fn out_diff(&mut self, normal: &str, compact: &str, can_put_semicolon: bool) {
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
  ) where F: FnMut(&I, &mut Emitter) -> bool {
    let separator = if self.is_compact {
      compact_separator
    } else {
      normal_separator
    };
    let mut remaining = vector.len();
    for part in vector {
      remaining -= 1;
      if !traversal_fn(part, self) { continue; }
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
pub fn emit_code(ast: ASTIndex, sp: &SourceProperties, compact: bool) -> String {
  let mut emitter = Emitter {
    output: String::new(),
    is_compact: compact,
    line_has_data: false,
    is_mid_line: false,
    indent_level: 0,
    curr_line: 0,
    sp,
  };

  // Handle the program `Block` node
  let node = unsafe { &*(sp.arena.get(ast) as *const ASTNode) };
  if let ASTNode::Block { nodes } = node {
    for node in nodes.iter() {
      emit_single(*node, &mut emitter);
      emitter.endline();
    }
  } else {
    panic!("Expected `Block` node in emit!");
  }

  emitter.finalize()
}

fn emit_single(
  ast: ASTIndex,
  emitter: &mut Emitter,
) {
  match unsafe { &*(emitter.sp.arena.get(ast) as *const ASTNode) } {
    ASTNode::Block { nodes } => {
      if nodes.is_empty() {
        emitter.out("{}", false);
      } else if nodes.len() == 1 {
        emitter.out_diff("{ ", "{", false);
        emit_single(nodes[0], emitter);
        emitter.out_diff(" }", "}", false);
      } else {
        emitter.out("{", false);
        emitter.endline();
        emitter.indent();
        for node in nodes {
          emit_single(*node, emitter);
          emitter.endline();
        }
        emitter.endline();
        emitter.unindent();
        emitter.out("}", false);
      }
    }
    ASTNode::VariableDeclaration {
      modifiers,
      def_type,
      defs
    } => {
      emitter.out(&modifiers.emit(true), false);
      emitter.out(&def_type.emit(), false);

      if emitter.is_compact && pattern_potentially_starts_with_symbol(&defs[0].name) {
        // Omit space!
      } else {
        emitter.out(" ", false);
      }
      emit_destructurable_declarations(defs.as_ref(), NO_REST.clone(), emitter);
      emitter.out("", true);
    }
    ASTNode::StatementImport { inner } => {
      match &**inner {
        ImportDefinition::DefaultAliased { source, alias } => {
          emitter.out("import ", false);
          emitter.out(&alias, false);
          emitter.out(" from ", true);
          emitter.out(&source, true);
        }
        ImportDefinition::AllAsAlias { source, alias } => {
          emitter.out("import ", false);
          emitter.out("* as ", false);
          emitter.out(&alias, false);
          emitter.out(" from ", true);
          emitter.out(&source, true);
        }
        ImportDefinition::Individual { source, parts } => {
          let has_imports = parts.iter().any(|i| {
            emitter.sp.st.lookup(&i.name).is_some_and(|s| s.is_used)
          });
          if has_imports {
            emitter.out("import ", false);
            emitter.out_diff("{ ", "{", false);
            emitter.emit_vec(parts.as_ref(), |i, emitter| {
              if emitter.sp.st.lookup(&i.name).is_some_and(|s| !s.is_used) {
                return false;
              }
              emitter.out(&i.name, false);
              if let Some(alias) = &i.alias {
                emitter.out(" as ", false);
                emitter.out(&alias, false);
              }
              true
            }, ", ", ",");
            emitter.out_diff(" }", "}", false);
            emitter.out_diff(" from ", "from", true);
            emitter.out(&source, true);
          }
        }
        ImportDefinition::SourceOnly { source } => {
          emitter.out("import ", false);
          emitter.out(&source, true);
        }
      }
    }
    ASTNode::ExpressionImport { value } => {
      emitter.out("import(", false);
      emit_single(*value, emitter);
      emitter.out(")", true);
    }
    ASTNode::StatementExport { inner } => {
      emitter.out_diff("export ", "export", false);
      emitter.out_diff("{ ", "{", false);
      emitter.emit_vec(inner.specifiers.as_ref(), |specifier, emitter| {
        if specifier.is_type {
          return false;
        }
        emitter.out(&specifier.name, false);
        if let Some(alias) = &specifier.alias {
          emitter.out(" as ", false);
          emitter.out(alias, false);
        }
        true
      }, ", ", ",");
      emitter.out_diff(" }", "}", true);
    }
    ASTNode::StatementIf { condition, body, alternate } => {
      let start_line = emitter.curr_line();

      // Head
      emitter.out_diff("if (", "if(", false);
      emit_single(*condition, emitter);
      emitter.out_diff(") ", ")", false);

      // Body
      emit_single(*body, emitter);

      // Else (if any)
      if let Some(alternate) = alternate {
        if emitter.is_compact && node_potentially_starts_with_symbol(*alternate, emitter.sp) {
          // Compact
          emitter.out("else", false);
        } else {
          if emitter.curr_line() == start_line {
            // If the if statement remained single-line, do a new line
            emitter.endline();
            emitter.out("else ", false);
          } else {
            // Otherwise, 
            emitter.out(" else ", false);
          }
        }
        emit_single(*alternate, emitter);
      }
    }
    ASTNode::StatementWhile { condition, body } => {
      // Head
      emitter.out_diff("while (", "while(", false);
      emit_single(*condition, emitter);
      emitter.out_diff(") ", ")", false);

      // Body
      emit_single(*body, emitter);
    }
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
    ASTNode::StatementForOf { init, expression, body } => {
      // Head
      emitter.out_diff("for (", "for(", false);
      emit_single(*init, emitter);
      emitter.out(" of ", false);
      emit_single(*expression, emitter);
      emitter.out_diff(") ", ")", false);

      // Body
      emit_single(*body, emitter);
    }
    ASTNode::StatementForIn { init, expression, body } => {
      // Head
      emitter.out_diff("for (", "for(", false);
      emit_single(*init, emitter);
      emitter.out(" in ", false);
      emit_single(*expression, emitter);
      emitter.out_diff(") ", ")", false);

      // Body
      emit_single(*body, emitter);
    }
    ASTNode::StatementSwitch { inner } => {
      emitter.out_diff("switch (", "switch(", false);
      emit_single(inner.condition, emitter);
      emitter.out_diff(") {", "){", false);
      emitter.endline();
      emitter.indent();

      for (case_cond, case_body) in inner.cases.iter() {
        if emitter.is_compact && node_potentially_starts_with_symbol(*case_cond, emitter.sp) {
          emitter.out("case", false);
        } else {
          emitter.out("case ", false);
        }
        emit_single(*case_cond, emitter);
        emitter.out_diff(": ", ":", false);
        emitter.endline();
        emitter.indent();
        for stmt in case_body.iter() {
          emit_single(*stmt, emitter);
          emitter.endline();
        }
        emitter.unindent();
      }

      if let Some(default_body) = &inner.default {
        emitter.out_diff("default: ", "default:", false);
        emitter.endline();
        emitter.indent();
        for stmt in default_body.iter() {
          emit_single(*stmt, emitter);
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
        let can_omit_space = node_potentially_starts_with_symbol(*value, emitter.sp);
        if emitter.is_compact && can_omit_space {
          // Omit space!
        } else {
          emitter.out(" ", false);
        }
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
    ASTNode::StatementTryCatchFinally { inner } => {
      emitter.out_diff("try ", "try", false);
      emit_single(inner.block_try, emitter);
      if let Some(block) = &inner.block_catch {
        emitter.out_diff(" catch ", "catch", false);
        if let Some(capture) = &inner.capture_catch {
          emitter.out("(", false);
          emitter.out(capture, false);
          emitter.out_diff(") ", ")", false);
        }
        emit_single(*block, emitter);
      }
      if let Some(block) = &inner.block_finally {
        emitter.out_diff(" finally ", "finally", false);
        emit_single(*block, emitter);
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
      // Async
      if arrow_fn.is_async {
        emitter.out_diff("async ", "async", false);
      }

      // Params
      let is_simple_param = match arrow_fn.params.as_ref() {
        [param] => match &param.name {
          DestructurePattern::Identifier { name } => {
            emitter.out(name, false);
            true
          },
          _ => false
        }
        _ => false
      };
      
      if !is_simple_param {
        emitter.out("(", false);
        emit_destructurable_declarations(
          arrow_fn.params.as_ref(),
          arrow_fn.rest.clone(), emitter
        );
        emitter.out(")", false);
      }

      // Arrow
      emitter.out_diff(" => ", "=>", false);

      // Body
      emit_single(arrow_fn.body, emitter);
      emitter.out("", true);
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
            let is_skippable = declaration.name.is_named() && declaration.value.is_none();
            if is_skippable && emitter.is_compact {
              continue;
            }
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
            emit_single(*body, emitter);
            emitter.endline();
          }
        }
      }

      emitter.endline();
      emitter.unindent();
      emitter.out("}", false)
    }
    ASTNode::Parenthesis { nodes } => {
      emitter.out("(", false);
      emitter.emit_vec(nodes.as_ref(), |node, emitter| {
        emit_single(*node, emitter);
        true
      }, ", ", ",");
      emitter.out(")", true);
    }
    ASTNode::Array { nodes } => {
      emitter.out_diff("[ ", "[", false);
      emitter.emit_vec(nodes.as_ref(), |node, emitter| {
        emit_single(*node, emitter);
        true
      }, ", ", ",");
      emitter.out_diff(" ]", "]", true);
    }
    ASTNode::Dict { properties } => {
      emitter.out_diff("{ ", "{", false);
      emitter.emit_vec(properties.as_ref(), |prop, emitter| {
        match prop {
          ObjectProperty::Rest { argument } => {
            emitter.out("...", false);
            emit_single(*argument, emitter);
          }
          ObjectProperty::Property { computed, key, value } => {
            if *computed { emitter.out("[", false); }
            emit_single(*key, emitter);
            if *computed { emitter.out("]", false); }
            emitter.out_diff(": ", ":", false);
            emit_single(*value, emitter);
          }
          ObjectProperty::Shorthand { key } => {
            emitter.out(key, false);
          }
        }
        true
      }, ", ", ",");
      emitter.out_diff(" }", "}", true);
    }
    ASTNode::ExprNumLiteral { number } => {
      let mut out = number.to_owned();
      if emitter.is_compact {
        let is_negative = if out.starts_with("-") {
          out = out[1..].to_owned();
          true
        } else { false };
        if out.starts_with("+") {
          out = out[1..].to_owned();
        }
        let is_bigint = if out.ends_with("n") {
          out = out[0..out.len() - 1].to_owned();
          true
        } else { false };

        if number.contains("0.") {
          // Shorten leading zero before decimal
          out = out[1..].to_owned();
        } else if number.starts_with("0b") {
          // Convert binary to decimal
          out = convert_binary_str_to_decimal_str(&out[2..]);
        } else if number.starts_with("0x") {
          // Convert hexadecimal to decimal
          out = convert_hexadecimal_str_to_decimal_str(&out[2..]);
        }

        while out.len() > 1 && out.starts_with("0") {
          out = out[1..].to_owned();
        }

        if !out.contains(".") {
          if let Some(shorter) = decimal_to_hex_if_shorter(&out) {
            out = shorter;
          }
        }

        // Put negative back
        if is_negative { out = "-".to_owned() + &out; }
        if is_bigint { out += "n"; }
      }
      emitter.out(&out, true);
    }
    ASTNode::ExprStrLiteral { string } => {
      emitter.out(&string, true);
    }
    ASTNode::ExprRegexLiteral { inner } => {
      emitter.out("/", false);
      emitter.out(&inner.pattern, false);
      emitter.out("/", false);
      emitter.out(&inner.flags, true);
    }
    ASTNode::ExprTemplateLiteral { inner } => {
      emitter.out("`", false);
      emitter.out(&inner.head, false);
      for (expr, literal_part) in &inner.parts {
        emitter.out("${", false);
        emit_single(*expr, emitter);
        emitter.out("}", false);
        emitter.out(literal_part, false);
      }
      emitter.out("`", true);
    }
    ASTNode::ExprIdentifier { name } => {
      emitter.out(&name, true);
    }
    ASTNode::ExprBoolLiteral { value } => {
      let offset = *value as u8 + ((emitter.is_compact as u8) << 1);
      emitter.out([
        "false",
        "true",
        "!1",
        "!0",
      ][offset as usize], true);
    }
    ASTNode::ExprFunctionCall { inner } => {
      emit_single(inner.callee, emitter);
      emitter.out("(", false);
      emitter.emit_vec(inner.arguments.as_ref(), |argument, emitter| {
        emit_single(*argument, emitter);
        true
      }, ", ", ",");
      emitter.out(")", true);
    }
    ASTNode::TemplateLiteralTag { callee, argument } => {
      emit_single(*callee, emitter);
      emit_single(*argument, emitter);
    }
    ASTNode::ExprIndexing { callee, property } => {
      emit_single(*callee, emitter);
      emitter.out("[", false);
      emit_single(*property, emitter);
      emitter.out("]", true);
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
    ASTNode::InfixOpr { left_right, opr } => {
      let can_have_spaces = match opr.as_str() {
        "." => (false, false),
        "?." => (false, false),
        "," => (false, true),
        _ => (true, true)
      };
      let needs_spaces = match opr.as_str() {
        "in" => true,
        "instanceof" => true,
        _ => false
      };
      emit_single(left_right.0, emitter);
      if needs_spaces || (can_have_spaces.0 && !emitter.is_compact) {
        emitter.out(" ", false);
      }
      emitter.out(&opr, false);
      if needs_spaces || (can_have_spaces.1 && !emitter.is_compact) {
        emitter.out(" ", false);
      }
      emit_single(left_right.1, emitter);
    }
    ASTNode::PostfixOpr { expr, opr } => {
      emit_single(*expr, emitter);
      emitter.out(&opr, true);
    }
    ASTNode::NonNullAssertion { expr } => {
      emit_single(*expr, emitter);
    }
    ASTNode::ExprAs { value, .. } => { emit_single(*value, emitter); }
    ASTNode::ExprTypeAssertion { value, .. } => { emit_single(*value, emitter); }
    ASTNode::EnumDeclaration { inner } => {
      emitter.out(&inner.modifiers.emit(true), false);
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
        emit_single(*value, emitter);
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

  // Don't put any code here.
  // Any return statements above will mess you up.
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
    true
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
      unsafe { declaration.name.get_computed_unchecked() },
      emitter
    );
    emitter.out("]", false);
  }
  if let Some(value) = &declaration.value {
    emitter.out_diff(" = ", "=", false);
    emit_single(*value, emitter);
  }
}

#[inline]
fn emit_single_variable_declaration(
  declaration: &DestructurableDeclaration,
  emitter: &mut Emitter
) {
  emit_destructure_pattern(&declaration.name, emitter);
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
        true
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
      emitter.emit_vec(properties.as_ref(), |(property, alias), emitter| {
        match (property, alias) {
          (DestructurePattern::Identifier { name: a }, DestructurePattern::Identifier { name: b }) if a == b => {
            emitter.out(a, true);
          },
          _ => {
            emit_destructure_pattern(property, emitter);
            emitter.out_diff(": ", ":", false);
            emit_destructure_pattern(alias, emitter);
          }
        }
        true
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
    DestructurePattern::NumericProperty { value } => {
      emitter.out(value, false);
    }
    DestructurePattern::StringProperty { value } => {
      emitter.out(value, false);
    }
    DestructurePattern::Ignore => {
      emitter.out("", true);
    }
    DestructurePattern::WithInitializer { pattern, initializer } => {
      emit_destructure_pattern(pattern, emitter);
      emitter.out_diff(" = ", "=", false);
      emit_single(*initializer, emitter);
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
    emit_single(*body, emitter);
  }
}

fn node_potentially_starts_with_symbol(
  node: ASTIndex,
  sp: &SourceProperties
) -> bool {
  match unsafe { &*(sp.arena.get(node) as *const ASTNode) } {
    ASTNode::ExprNumLiteral { number } => number.starts_with("."),
    ASTNode::ExprStrLiteral { .. } => true,
    ASTNode::ExprTemplateLiteral { .. } => true,
    ASTNode::Array { .. } => true,
    ASTNode::Dict { .. } => true,
    ASTNode::Block { .. } => true,

    ASTNode::PrefixOpr { opr, .. } => !opr.chars().next().unwrap().is_alphanumeric(),
    ASTNode::InfixOpr { left_right, .. } => node_potentially_starts_with_symbol(left_right.0, sp),
    ASTNode::PostfixOpr { expr, .. } => node_potentially_starts_with_symbol(*expr, sp),
    ASTNode::Parenthesis { .. } => true,

    _ => false
  }
}

fn pattern_potentially_starts_with_symbol(pattern: &DestructurePattern) -> bool {
  match pattern {
    DestructurePattern::Array { .. } => true,
    DestructurePattern::Object { .. } => true,
    DestructurePattern::WithInitializer { pattern, .. } => pattern_potentially_starts_with_symbol(pattern),
    _ => false
  }
}

fn convert_binary_str_to_decimal_str(binary: &str) -> String {
  let mut decimal = String::from("0");
  for bit in binary.chars() {
    // Multiply current decimal by 2
    let mut carry = 0;
    let mut new_decimal = String::new();
    for digit_char in decimal.chars().rev() {
      let digit = digit_char.to_digit(10).unwrap();
      let product = digit * 2 + carry;
      new_decimal.push(std::char::from_digit(product % 10, 10).unwrap());
      carry = product / 10;
    }
    if carry > 0 {
      new_decimal.push(std::char::from_digit(carry, 10).unwrap());
    }
    decimal = new_decimal.chars().rev().collect();

    // Add current bit
    if bit == '1' {
      let mut carry = 1;
      let mut new_decimal = String::new();
      for digit_char in decimal.chars().rev() {
        let digit = digit_char.to_digit(10).unwrap();
        let sum = digit + carry;
        new_decimal.push(std::char::from_digit(sum % 10, 10).unwrap());
        carry = sum / 10;
      }
      if carry > 0 {
        new_decimal.push(std::char::from_digit(carry, 10).unwrap());
      }
      decimal = new_decimal.chars().rev().collect();
    }
  }
  decimal
}

fn convert_hexadecimal_str_to_decimal_str(hex: &str) -> String {
  let mut decimal = String::from("0");
  for hex_char in hex.chars() {
    let hex_value = hex_char.to_digit(16).unwrap();

    // Multiply current decimal by 16
    let mut carry = 0;
    let mut new_decimal = String::new();
    for digit_char in decimal.chars().rev() {
      let digit = digit_char.to_digit(10).unwrap();
      let product = digit * 16 + carry;
      new_decimal.push(std::char::from_digit(product % 10, 10).unwrap());
      carry = product / 10;
    }
    while carry > 0 {
      new_decimal.push(std::char::from_digit(carry % 10, 10).unwrap());
      carry /= 10;
    }
    decimal = new_decimal.chars().rev().collect();

    // Add hex value
    let mut carry = hex_value;
    let mut new_decimal = String::new();
    for digit_char in decimal.chars().rev() {
      let digit = digit_char.to_digit(10).unwrap();
      let sum = digit + carry;
      new_decimal.push(std::char::from_digit(sum % 10, 10).unwrap());
      carry = sum / 10;
    }
    while carry > 0 {
      new_decimal.push(std::char::from_digit(carry % 10, 10).unwrap());
      carry /= 10;
    }
    decimal = new_decimal.chars().rev().collect();
  }
  decimal
}

pub fn decimal_to_hex_if_shorter(input: &str) -> Option<String> {
  if input.is_empty() || !input.bytes().all(|b| b.is_ascii_digit()) {
    return None;
  }

  let dec_len = input.len();

  if input.bytes().all(|b| b == b'0') {
    let hex = "0x0".to_string();
    return (hex.len() < dec_len).then_some(hex);
  }

  let mut digits: Vec<u8> = input.bytes().map(|b| b - b'0').collect();
  let mut hex_nibbles = Vec::new();

  while !digits.is_empty() {
    let mut carry: u32 = 0;
    let mut next = Vec::with_capacity(digits.len());

    for &d in &digits {
      let cur = carry * 10 + d as u32;
      let q = (cur / 16) as u8;
      carry = cur % 16;
      if !next.is_empty() || q != 0 {
        next.push(q);
      }
    }

    hex_nibbles.push(carry as u8);
    digits = next;
  }

  const HEX: &[u8; 16] = b"0123456789abcdef";
  let mut hex = String::with_capacity(hex_nibbles.len() + 2);
  hex.push_str("0x");
  for &n in hex_nibbles.iter().rev() {
    hex.push(HEX[n as usize] as char);
  }

  (hex.len() < dec_len).then_some(hex)
}

