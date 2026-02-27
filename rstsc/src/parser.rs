use phf::{phf_map, phf_set};
use crate::ast::{ASTIndex, ASTNode, ArrowFunctionDefinition, ClassDefinition, ClassMember, EnumDeclaration, ExprFunctionCall, ExprRegexLiteral, ExprTemplateLiteral, FunctionDefinition, GetterSetter, ImportDefinition, IndividualImport, InterfaceDeclaration, ObjectProperty, Switch, TryCatchFinally};
use crate::ast_common::{ACCESSIBILITY_MODIFIERS, MODIFIERS, Modifier, ModifierList, VariableDefType};
use crate::declaration::{ComputableDeclarationName, Declaration, DeclarationComputable, DeclarationTyped, DestructurableDeclaration, DestructurePattern};
use crate::error_type::CompilerError;
use crate::operations::{ARROW_FN_PRECEDENCE, COMMA_PRECEDENCE, ExprType, TEMPLATE_LITERAL_TAG_PRECEDENCE, get_operator_binding_power};
use crate::rest::Rest;
use crate::small_vec::SmallVec;
use crate::source_properties::{SourceProperties, SrcMapping};
use crate::symbol_table::{Symbol, SymbolOrigin};
use crate::tokenizer::TokenType;
use crate::type_infer::infer_types;
use crate::types::{ObjectSquareBracketReturn, Type, get_comma_separated_types_until, get_generics, get_optional_generics, get_type, parse_object_square_bracket, try_get_type};

pub static INVERSE_GROUPINGS: phf::Map<&'static str, &'static str> = phf_map! {
  "(" => ")",
  "[" => "]",
  "{" => "}",
  "<" => ">",
  
  ")" => "(",
  "]" => "[",
  "}" => "{",
  ">" => "<",
};

const VARIABLE_DECLARATIONS: &[&str] = &[ "var", "let", "const" ];
const IGNORE_MODIFIER_IF_SYMBOL: &[&str] = &[ "(", "<", ":", "=" ];

pub static ANONYMOUS_CLASS_NAME: &str = "\0";

pub static DISALLOWED_VARIABLE_NAMES: phf::Set<&'static str> = phf_set! {
  "abstract", "arguments", "await", "boolean", "break", "byte", "case", "catch",
  "char", "class", "const", "continue", "debugger", "default", "delete", "do",
  "double", "else", "enum", "eval", "export", "extends", "false", "final",
  "finally", "float", "for", "function", "goto", "if", "implements", "import",
  "in", "instanceof", "int", "interface", "let", "long", "native", "new",
  "null", "package", "private", "protected", "public", "return", "short",
  "static", "super", "switch", "synchronized", "this", "throw", "throws",
  "transient", "true", "try", "typeof", "var", "void", "volatile", "while",
  "with", "yield",
};

/// Parses a single block. Consumes the ending token, but not the starting token.
pub fn get_block(sp: &mut SourceProperties) -> Result<ASTIndex, CompilerError> {
  let arena_idx = sp.nodes.add(ASTNode::Empty);
  let mut nodes = SmallVec::new();

  // Go through the sp.tokens list
  loop {
    nodes.push(get_single_statement(sp)?);

    if sp.tokens.peek_str() == ";" { sp.tokens.skip_unchecked(); continue; }
    if sp.tokens.is_done() { break; }
    if [ ")", "]", "}" ].contains(&sp.tokens.peek_str()) { sp.tokens.skip_unchecked(); break; }
  }

  // Remove trailing empty
  if nodes.last().is_some_and(|i| matches!(*sp.nodes.get(*i), ASTNode::Empty)) {
    nodes.pop();
  }

  // Return the program node
  sp.nodes.set(arena_idx, ASTNode::Block { nodes });
  Ok(arena_idx)
}

/// Gets a single statement, until it reaches either a `;` or a group end
/// (eg. `)]}`). Only consumes semicolons, not group ends. If a group start is
/// reached (eg. `([{`), calls the corresponding function for said group.
fn get_single_statement<'a, 'b>(
  sp: &mut SourceProperties
) -> Result<ASTIndex, CompilerError> where 'a: 'b {
  sp.tokens.ignore_whitespace();
  if sp.tokens.is_done() {
    return Ok(sp.nodes.add(ASTNode::Empty));
  }
  let ret = match sp.tokens.peek_str() {
    "{" => handle_block(sp)?.unwrap(),
    "var" | "let" | "const" => handle_var(sp)?.unwrap(),
    "if" | "while" | "for" | "switch" => handle_control_flow(sp)?.unwrap(),
    "function" => handle_function_declaration(sp)?.unwrap(),
    "class" => handle_class_declaration(sp)?.unwrap(),
    "import" => handle_import(sp)?.unwrap(),
    "export" | "async" | "static" | "public" | "private" | "protected" | "readonly" | "abstract" | "override" => 
        handle_modifiers(sp)?.unwrap(),
    "return" | "break" | "continue" | "throw" => handle_other_statements(sp)?.unwrap(),
    "type" => handle_type_declaration(sp)?.unwrap(),
    "enum" => handle_enum(false, sp)?.unwrap(),
    "interface" => handle_interface(sp)?.unwrap(),
    "try" => handle_try_catch(sp)?.unwrap(),
    "declare" => handle_declare(sp)?.unwrap(),
    _ => handle_expression(sp)?,
  };

  sp.tokens.ignore_whitespace();
  if sp.tokens.peek_str() == ";" {
    sp.tokens.skip_unchecked();
  }

  Ok(ret)
}

/// Handles blocks (defined as a non-expression `{` token)
fn handle_block(
  sp: &mut SourceProperties
) -> Result<Option<ASTIndex>, CompilerError> {
  if sp.tokens.peek_str() != "{" {
    return Ok(None);
  }
  sp.tokens.skip_unchecked(); // Skip "{"
  Ok(Some(get_block(sp)?))
}

/// Handles variable initialization
fn handle_var<'a, 'b>(
  sp: &mut SourceProperties
) -> Result<Option<ASTIndex>, CompilerError> where 'a: 'b {
  if !VARIABLE_DECLARATIONS.contains(&sp.tokens.peek_str()) {
    return Ok(None);
  }

  let def_type = get_variable_def_type(sp)?;

  if matches!(def_type, VariableDefType::Const) && sp.tokens.peek_str() == "enum" {
    // Const enums
    return handle_enum(true, sp);
  }

  // Get the variable definitions
  let (defs, _) = get_multiple_destructurable_declarations(false, sp)?;

  Ok(Some(sp.nodes.add(ASTNode::VariableDeclaration {
    modifiers: ModifierList::new(),
    def_type,
    defs
  })))
}

fn get_variable_def_type<'b>(
  sp: &mut SourceProperties,
) -> Result<VariableDefType, CompilerError> {
  // Get the header
  let header_token = sp.tokens.consume();
  sp.tokens.ignore_whitespace();
  match sp.str_src(header_token.value) {
    "var"   => Ok(VariableDefType::Var),
    "let"   => Ok(VariableDefType::Let),
    "const" => Ok(VariableDefType::Const),
    other => Err(CompilerError::new(
      header_token.value,
      format!("Unexpected variable declaration: {}", other),
    ))
  }
}

/// Gets a single named (and optionally typed) declaration,
/// with or without a value. Does NOT handle rest parameters!
fn get_declaration<'a, 'b>(
  sp: &mut SourceProperties,
) -> Result<Declaration, CompilerError> where 'a: 'b {
  sp.tokens.ignore_whitespace();
  let name = sp.tokens.consume_type(TokenType::Identifier)?.value.to_owned();
  let (typ, value) = get_declaration_after_name(sp)?;

  // Create symbol for this declaration
  let symbol = Symbol::new(
    sp.str_src(name),
    SymbolOrigin::Variable,
    if let Some(value) = &value {
      infer_types(*value, sp)
    } else {
      Type::Unknown
    }
  );
  sp.st.add_symbol(symbol)?;
  
  Ok(Declaration::new(name, typ, value))
}

fn get_declaration_after_name<'a, 'b>(
  sp: &mut SourceProperties,
) -> Result<(Type, Option<ASTIndex>), CompilerError> where 'a: 'b {
  let typ = if sp.tokens.try_skip_and_ignore_whitespace("?") {
    if sp.tokens.peek_str() != ":" {
      return Err(CompilerError::new(
        sp.tokens.consume().value,
        "Expected `:` after `?` in conditional declaration".to_owned()
      ))
    }
    let mut typ = get_type(sp)?;
    typ.intersection(Type::Void);
    Some(typ)
  } else if sp.tokens.try_skip_and_ignore_whitespace("!") {
    if sp.tokens.peek_str() != ":" {
      return Err(CompilerError::new(
        sp.tokens.consume().value,
        "Expected `:` after `!` in non-null asserted declaration".to_owned(),
      ))
    }
    Some(get_type(sp)?)
  } else {
    try_get_type(sp)?
  }.unwrap_or(Type::Unknown);

  sp.tokens.ignore_whitespace();
  let value = if sp.tokens.peek_str() == "=" {
    sp.tokens.skip_unchecked();
    Some(
      get_expression(
        *crate::operations::COMMA_PRECEDENCE,
        sp
      )?
    )
  } else {
    None
  };

  Ok((typ, value))
}

fn get_destructurable_declaration<'a, 'b>(
  sp: &mut SourceProperties,
) -> Result<DestructurableDeclaration, CompilerError> where 'a: 'b {
  let name = parse_destructure_pattern(false, sp)?;
  let (typ, initializer) = get_declaration_after_name(sp)?;
  
  // Create symbol for identifier patterns
  if let DestructurePattern::Identifier { name: identifier_name } = &name {
    let symbol = Symbol::new(
      sp.str_src(*identifier_name),
      SymbolOrigin::Variable,
      if let Some(init) = &initializer {
        infer_types(*init, sp)
      } else {
        Type::Unknown
      }
    );
    sp.st.add_symbol(symbol)?;
  }
  
  Ok(DestructurableDeclaration {
    name: if let Some(initializer) = initializer {
      DestructurePattern::WithInitializer { pattern: Box::new(name), initializer }
    } else { name },
    typ
  })
}

fn get_multiple_destructurable_declarations<'a, 'b>(
  allow_rest: bool,
  sp: &mut SourceProperties,
) -> Result<(SmallVec<DestructurableDeclaration>, Rest), CompilerError> where 'a: 'b {
  sp.tokens.ignore_whitespace();
  let mut declarations: SmallVec<DestructurableDeclaration> = SmallVec::new();
  let mut rest = Rest::new();
  while ![ ";", ")" ].contains(&sp.tokens.peek_str()) {
    rest.try_set(sp, allow_rest)?;
    declarations.push(get_destructurable_declaration(sp)?);
    if !sp.tokens.ignore_commas() { break }
  }
  Ok((declarations, rest))
}

fn parse_destructure_pattern(
  capture_initializer: bool,
  sp: &mut SourceProperties,
) -> Result<DestructurePattern, CompilerError> {
  sp.tokens.ignore_whitespace();
  let pattern = match sp.tokens.peek_str() {
    "[" => {
      // Array destructure
      sp.tokens.skip_unchecked(); // Skip "["
      let mut elements = SmallVec::new();
      let mut spread = None;
      while sp.tokens.peek_str() != "]" {
        sp.tokens.ignore_whitespace();
        if sp.tokens.peek_str() == "..." {
          if spread.is_some() {
            return Err(CompilerError::new(
              sp.tokens.consume().value,
              "Only one spread allowed in array destructure".to_owned(),
            ))
          }
          sp.tokens.skip_unchecked();
          spread = Some(Box::new(parse_destructure_pattern(false, sp)?));
        } else if sp.tokens.peek_str() == "," {
          if spread.is_some() {
            return Err(CompilerError::new(
              sp.tokens.consume().value,
              "A spread may not have a trailing comma".to_owned(),
            ))
          }
          while sp.tokens.try_skip_and_ignore_whitespace(",") {
            elements.push(DestructurePattern::Ignore);
          }
        } else {
          if spread.is_some() {
            return Err(CompilerError::new(
              sp.tokens.consume().value,
              "A spread must be last in a destructuring pattern".to_owned(),
            ))
          }
          elements.push(parse_destructure_pattern(true, sp)?);
        }
        sp.tokens.ignore_whitespace();
        if sp.tokens.peek_str() == "," {
          sp.tokens.skip_unchecked();
        }
      }
      sp.tokens.skip_unchecked(); // Skip "]"
      sp.tokens.ignore_whitespace();
      DestructurePattern::Array { elements, spread }
    },
    "{" => {
      // Object destructure
      sp.tokens.skip_unchecked(); // Skip "{"
      let mut properties = SmallVec::new();
      let mut rest = None;
      while sp.tokens.peek_str() != "}" {
        sp.tokens.ignore_whitespace();
        if sp.tokens.peek_str() == "..." {
          if rest.is_some() {
            return Err(CompilerError::new(
              sp.tokens.consume().value,
              "Only one rest element allowed in object destructure".to_owned(),
            ))
          }
          sp.tokens.skip_unchecked();
          rest = Some(Box::new(parse_destructure_pattern(false, sp)?));
          if sp.tokens.peek_str() == "=" {
            return Err(CompilerError::new(
              sp.tokens.consume().value,
              "A rest element cannot have an initializer".to_owned(),
            ))
          }
        } else if sp.tokens.peek_str() == "," {
          if rest.is_some() {
            return Err(CompilerError::new(
              sp.tokens.consume().value,
              "A rest element may not have a trailing comma".to_owned(),
            ))
          }
          sp.tokens.skip_unchecked();
        } else {
          if rest.is_some() {
            return Err(CompilerError::new(
              sp.tokens.consume().value,
              "A spread must be last in a destructuring pattern".to_owned(),
            ))
          }
          let property = parse_destructure_pattern(false, sp)?;
          let needs_alias = match property {
            DestructurePattern::NumericProperty { .. } => true,
            DestructurePattern::StringProperty { .. } => true,
            _ => false
          };
          let alias = if sp.tokens.peek_str() == ":" {
            sp.tokens.skip_unchecked();
            parse_destructure_pattern(false, sp)?
          } else if needs_alias {
            return Err(CompilerError::new(
              sp.tokens.consume().value,
              "Expected `:`".to_owned(),
            ));
          } else {
            property.clone()
          };
          let alias = try_parse_destructure_pattern_initializer(alias, sp)?;
          sp.tokens.ignore_whitespace();
          properties.push((property, alias));
        }
        sp.tokens.ignore_whitespace();
        if sp.tokens.peek_str() == "," {
          sp.tokens.skip_unchecked();
        }
      }
      sp.tokens.skip_unchecked(); // Skip "}"
      sp.tokens.ignore_whitespace();
      DestructurePattern::Object { properties, spread: rest }
    },
    _ => {
      // Identifier, numeric property, or string property
      let token = sp.tokens.consume();
      let out = match token.typ {
        TokenType::Identifier => DestructurePattern::Identifier { name: token.value.to_owned() },
        TokenType::Number => DestructurePattern::NumericProperty { value: token.value.to_owned() },
        TokenType::String => DestructurePattern::StringProperty { value: token.value.to_owned() },
        _ => return Err(CompilerError::new(
          token.value,
          "Expected Identifier, Number or String in destructure pattern".to_owned(),
        ))
      };
      sp.tokens.ignore_whitespace();
      out
    }
  };

  if capture_initializer {
    try_parse_destructure_pattern_initializer(pattern, sp)
  } else {
    Ok(pattern)
  }
}

#[inline]
fn try_parse_destructure_pattern_initializer(
  curr_pattern: DestructurePattern,
  sp: &mut SourceProperties,
) -> Result<DestructurePattern, CompilerError> {
  if sp.tokens.peek_str() == "=" {
    sp.tokens.skip_unchecked();
    Ok(DestructurePattern::WithInitializer {
      pattern: Box::new(curr_pattern),
      initializer: get_expression(*COMMA_PRECEDENCE, sp)?
    })
  } else {
    Ok(curr_pattern)
  }
}

/// Handles control flow, like `if`, `while`, and `for`
fn handle_control_flow(
  sp: &mut SourceProperties,
) -> Result<Option<ASTIndex>, CompilerError> {
  const CONTROL_FLOW: &[&str] = &[ "if", "while", "for", "switch" ];
  if !CONTROL_FLOW.contains(&sp.tokens.peek_str()) {
    return Ok(None);
  }

  let control_flow_type = sp.tokens.consume().value;
  let control_flow_type = sp.str_src(control_flow_type);
  sp.tokens.ignore_whitespace();

  Ok(Some(match control_flow_type {
    "if" | "while" => {
      // Get condition
      sp.tokens.skip("(")?;
      let condition = get_expression(0, sp)?;
      sp.tokens.skip_with_whitespace(")")?;
      
      // Get body
      let body = get_single_statement(sp)?;

      // Add to parent
      if control_flow_type == "if" {
        // Check for `else` block...
        sp.tokens.ignore_whitespace();

        let alternate = if sp.tokens.peek_str() == "else" {
          sp.tokens.skip_unchecked();
          Some(get_single_statement(sp)?)
        } else {
          None
        };
        sp.nodes.add(ASTNode::StatementIf { condition, body, alternate })
      } else {
        sp.nodes.add(ASTNode::StatementWhile { condition, body })
      }
    },
    "for" => handle_for_loop(sp)?,
    "switch" => {
      // Get condition
      sp.tokens.skip("(")?;
      let condition = get_expression(0, sp)?;
      sp.tokens.skip_with_whitespace(")")?;
      sp.tokens.skip("{")?;

      let mut cases = SmallVec::new();
      let mut default = None;

      while sp.tokens.peek_str() != "}" {
        sp.tokens.ignore_whitespace();
        if sp.tokens.peek_str() == "case" {
          sp.tokens.skip_unchecked();
          let case_condition = get_expression(0, sp)?;
          sp.tokens.skip_with_whitespace(":")?;
          let mut case_body = SmallVec::new();
          while !sp.tokens.is_done() && ![ "case", "default", "}" ].contains(&sp.tokens.peek_str()) {
            case_body.push(get_single_statement(sp)?);
            sp.tokens.ignore_whitespace();
          }
          cases.push((case_condition, case_body));
        } else if sp.tokens.peek_str() == "default" {
          if default.is_some() {
            return Err(CompilerError::new(
              sp.tokens.consume().value,
              "Switch statements can only have one default case".to_owned(),
            ))
          }
          sp.tokens.skip_unchecked();
          sp.tokens.ignore_whitespace();
          sp.tokens.skip(":")?;
          let mut default_body = SmallVec::new();
          while !sp.tokens.is_done() && ![ "case", "}" ].contains(&sp.tokens.peek_str()) {
            default_body.push(get_single_statement(sp)?);
            sp.tokens.ignore_whitespace();
          }
          default = Some(default_body);
        } else {
          return Err(CompilerError::new(
            sp.tokens.consume().value,
            "Expected `case` or `default` in switch statement".to_owned(),
          ))
        }
      }

      sp.tokens.skip_unchecked(); // Skip "}"

      sp.nodes.add(ASTNode::StatementSwitch { inner: Box::new(Switch {
        condition, cases, default
      }) })
    }
    other => { panic!("Control flow not implemented: {}", other); }
  }))
}

fn handle_for_loop(
  sp: &mut SourceProperties,
) -> Result<ASTIndex, CompilerError> {
  sp.tokens.skip("(")?;
  sp.tokens.ignore_whitespace();
  let init = if sp.tokens.peek_str() == ";" {
    sp.nodes.add(ASTNode::Empty)
  } else if VARIABLE_DECLARATIONS.contains(&sp.tokens.peek_str()) {
    let def_typ = get_variable_def_type(sp)?;
    let defs = get_multiple_destructurable_declarations(false, sp)?.0;
    sp.nodes.add(ASTNode::VariableDeclaration {
      modifiers: ModifierList::new(),
      def_type: def_typ,
      defs
    })
  } else {
    get_expression(0, sp)?
  };
  sp.tokens.ignore_whitespace();
  Ok(if sp.tokens.try_skip_and_ignore_whitespace(";") {
    let condition = get_single_statement(sp)?;
    let update = get_single_statement(sp)?;

    sp.tokens.skip(")")?;
    let body = get_single_statement(sp)?;

    sp.nodes.add(ASTNode::StatementFor { init, condition, update, body })
  } else if sp.tokens.try_skip_and_ignore_whitespace("of") {
    let expression = get_expression(0, sp)?;

    sp.tokens.skip(")")?;
    let body = get_single_statement(sp)?;

    sp.nodes.add(ASTNode::StatementForOf { init, expression, body })
  } else if sp.tokens.try_skip_and_ignore_whitespace("in") {
    let expression = get_expression(0, sp)?;
    
    sp.tokens.skip(")")?;
    let body = get_single_statement(sp)?;

    sp.nodes.add(ASTNode::StatementForIn { init, expression, body })
  } else {
    return Err(CompilerError::new(
      sp.tokens.peek().value,
      "Expected `;`, `of`, or `in` in for loop".to_owned(),
    ))
  })
}

/// Handles import statements
fn handle_import(
  sp: &mut SourceProperties,
) -> Result<Option<ASTIndex>, CompilerError> {
  if sp.tokens.peek_str() != "import" {
    return Ok(None);
  }
  let checkpoint = sp.tokens.get_checkpoint();
  sp.tokens.skip_unchecked();
  sp.tokens.ignore_whitespace();

  if sp.tokens.try_skip_and_ignore_whitespace("(") {
    sp.tokens.restore_checkpoint(checkpoint);
    return Ok(Some(get_expression(0, sp)?));
  } else {
    sp.tokens.ignore_checkpoint(checkpoint);
  }

  let mut has_distinction = 0;

  // Default imports `import Defaults from '...'`
  let default_alias = if sp.tokens.peek().typ == TokenType::Identifier {
    let alias = Some(sp.tokens.consume().value.to_owned());
    sp.tokens.ignore_whitespace();
    let _ = sp.tokens.try_skip_and_ignore_whitespace(",");
    has_distinction += 1;
    alias
  } else {
    None
  };

  // Wildcard imports `import * as Identifier from '...'`
  let wildcard = if sp.tokens.peek_str() == "*" {
    sp.tokens.skip_unchecked();
    sp.tokens.skip_with_whitespace("as")?;
    has_distinction += 1;
    let name = sp.tokens.consume_type(TokenType::Identifier)?.value.to_owned();
    sp.st.add_symbol(Symbol::new(
      sp.str_src(name),
      SymbolOrigin::Import,
      Type::Unknown
    ))?;
    Some(name)
  } else {
    None
  };
  sp.tokens.ignore_whitespace();

  // Individual imports `import { A, B } from '...'`
  let mut individual = SmallVec::new();
  if sp.tokens.peek_str() == "{" {
    sp.tokens.skip_unchecked();
    sp.tokens.ignore_whitespace();
    while !sp.tokens.is_done() && sp.tokens.peek_str() != "}" {
      let name = sp.tokens.consume_type(TokenType::Identifier)?.value.to_owned();
      let alias = if sp.tokens.peek_str() == "as" {
        sp.tokens.skip_unchecked();
        sp.tokens.ignore_whitespace();
        Some(sp.tokens.consume_type(TokenType::Identifier)?.value.to_owned())
      } else { None };
      sp.tokens.ignore_whitespace();
      let _ = sp.tokens.try_skip_and_ignore_whitespace(",");
      let final_name = sp.str_src(alias.clone().unwrap_or(name.clone()));
      sp.st.add_symbol(Symbol::new(
        final_name,
        SymbolOrigin::Import,
        Type::Unknown
      ))?;
      individual.push(IndividualImport { name, alias });
    }
    sp.tokens.skip("}")?;
    has_distinction += 1;
  }
  sp.tokens.ignore_whitespace();

  if has_distinction > 1 {
    return Err(CompilerError::new(
      sp.tokens.consume().value,
      "Can't mix multiple import types!".to_owned()
    ));
  } else if has_distinction > 0 {
    sp.tokens.skip("from")?;
    sp.tokens.ignore_whitespace();
  }
  let source = sp.tokens.consume_type(TokenType::String)?.value.to_owned();

  Ok(Some(sp.nodes.add(ASTNode::StatementImport { inner: Box::new(if let Some(default_alias) = default_alias {
    ImportDefinition::DefaultAliased { source, alias: default_alias }
  } else if let Some(wildcard) = wildcard {
    ImportDefinition::AllAsAlias { source, alias: wildcard }
  } else if individual.len() > 0 {
    ImportDefinition::Individual { source, parts: individual }
  } else {
    ImportDefinition::SourceOnly { source }
  }) })))
}

/// Handles `return`, `break`, `continue`, and `throw`
fn handle_other_statements(
  sp: &mut SourceProperties,
) -> Result<Option<ASTIndex>, CompilerError> {
  const STATEMENT_NAMES: &[&str] = &[
    "return",
    "break",
    "continue",
    "throw",
  ];
  if !STATEMENT_NAMES.contains(&sp.tokens.peek_str()) {
    return Ok(None);
  }

  let statement_type = sp.tokens.consume().value;
  sp.tokens.ignore_whitespace();

  let value = if sp.tokens.peek_str() != ";" {
    Some(get_expression(0, sp)?)
  } else {
    None
  };

  Ok(Some(match sp.str_src(statement_type) {
    "return" => sp.nodes.add(ASTNode::StatementReturn { value }),
    "break" => sp.nodes.add(ASTNode::StatementBreak { value }),
    "continue" => sp.nodes.add(ASTNode::StatementContinue { value }),
    "throw" => sp.nodes.add(ASTNode::StatementThrow { value }),
    other => { panic!("Statement not implemented: {}", other); }
  }))
}

fn handle_function_declaration<'a, 'b>(
  sp: &mut SourceProperties,
) -> Result<Option<ASTIndex>, CompilerError> where 'a: 'b {
  if sp.tokens.peek_str() != "function" {
    return Ok(None);
  }

  sp.tokens.skip_unchecked(); // Skip `function`

  // Get name
  sp.tokens.ignore_whitespace();
  let name = if sp.tokens.peek().typ == TokenType::Identifier {
    // Named function
    Some(sp.tokens.consume().value)
  } else {
    // Unnamed function
    None
  };

  let func = get_function_after_name(name, sp)?;
  Ok(Some(sp.nodes.add(ASTNode::FunctionDefinition {
    inner: Box::new(func)
  })))
}

/// Gets a function once the name has been consumed.
/// This includes any generics, arguments, return type, and body.
fn get_function_after_name<'a, 'b>(
  name: Option<SrcMapping>,
  sp: &mut SourceProperties,
) -> Result<FunctionDefinition, CompilerError> where 'a: 'b {
  sp.tokens.ignore_whitespace();

  let generics = get_optional_generics(sp)?;
  
  // Get parameters and return type
  sp.tokens.ignore_whitespace();
  sp.tokens.skip("(")?;

  // Create a new symbol table for function parameters and body
  sp.st.up_scope();
  let params = get_multiple_destructurable_declarations(true, sp)?;
  sp.tokens.skip(")")?;
  let return_type = try_get_type(sp)?;

  // Get body
  sp.tokens.ignore_whitespace();
  let body = if sp.tokens.peek_str() == ";" {
    // No body! This means it's just a declaration.
    None
  } else {
    sp.tokens.skip("{")?;
    Some(get_block(sp)?)
  };

  sp.st.down_scope();

  Ok(FunctionDefinition {
    modifiers: ModifierList::new(),
    name,
    generics,
    params: params.0,
    rest: params.1,
    return_type,
    body
  })
}

/// Similar to `get_function_after_name`, gets a class constructor.
fn get_constructor_after_name<'a, 'b>(
  members: &mut SmallVec<ClassMember>,
  sp: &mut SourceProperties,
) -> Result<FunctionDefinition, CompilerError> where 'a: 'b {
  sp.tokens.ignore_whitespace();

  // Handle generics
  if sp.tokens.peek_str() == "<" {
    return Err(CompilerError::new(
      sp.tokens.consume().value,
      "Type parameters cannot appear on a constructor declaration.".to_owned(),
    ))
  }
  
  // Get parameters
  let mut params: SmallVec<DestructurableDeclaration> = SmallVec::new();
  let mut set_properties: SmallVec<ASTIndex> = SmallVec::new();
  let mut rest = Rest::new();
  sp.tokens.skip("(")?;
  while sp.tokens.peek_str() != ")" {
    sp.tokens.ignore_whitespace();
    rest.try_set(sp, true)?;
    if ACCESSIBILITY_MODIFIERS.contains(&sp.tokens.peek_str()) {
      let modifiers = fetch_modifier_list(sp);
      let mut declaration = get_declaration(sp)?;
      params.push(declaration.clone().into());
      declaration.clear_value();
      members.push(ClassMember::Property(
        DeclarationComputable::from(&declaration), modifiers
      ));
      let this_name = sp.add_custom_string("this");
      let this_node = sp.nodes.add(ASTNode::ExprIdentifier { name: this_name });
      let decl_node = sp.nodes.add(ASTNode::ExprIdentifier { name: declaration.name });
      let dot_opr = sp.add_custom_string(".");
      let left_right = (
        sp.nodes.add(ASTNode::InfixOpr {
          left_right: (this_node, decl_node),
          opr: dot_opr,
        }),
        sp.nodes.add(ASTNode::ExprIdentifier { name: declaration.name }),
      );
      let eq_opr = sp.add_custom_string("=");
      set_properties.push(sp.nodes.add(ASTNode::InfixOpr {
        left_right, opr: eq_opr,
      }))
    } else {
      params.push(get_destructurable_declaration(sp)?);
    }
    sp.tokens.ignore_whitespace();
    if !sp.tokens.try_skip_and_ignore_whitespace(",") {
      break;
    }
  }
  sp.tokens.skip(")")?;

  // Get return type
  sp.tokens.ignore_whitespace();
  let return_type = try_get_type(sp)?;

  // Get body
  sp.tokens.ignore_whitespace();
  let body = if sp.tokens.peek_str() == ";" {
    // No body! This means it's just a declaration.
    None
  } else {
    sp.tokens.skip("{")?;
    let body = get_block(sp)?;
    match sp.nodes.get_mut(body) {
      ASTNode::Block { nodes } => {
        nodes.append_front(&mut set_properties);
        Some(body)
      }
      _ => {
        return Err(CompilerError::new(
          sp.nodes.get_node_src_range(body),
          "Expected block in function body".to_owned()
        ))
      }
    }
  };

  Ok(FunctionDefinition {
    modifiers: ModifierList::new(),
    name: Some(sp.add_custom_string("constructor")),
    generics: SmallVec::new(),
    params,
    rest,
    return_type,
    body
  })
}

fn handle_class_declaration<'a, 'b>(
  sp: &mut SourceProperties,
) -> Result<Option<ASTIndex>, CompilerError> where 'a: 'b {
  if sp.tokens.peek_str() != "class" {
    return Ok(None);
  }
  Ok(Some(get_class_expression(sp)?))
}

fn get_class_expression<'a, 'b>(
  sp: &mut SourceProperties,
) -> Result<ASTIndex, CompilerError> where 'a: 'b {
  let class_start = sp.tokens.consume(); // Skip "class"
  sp.tokens.ignore_whitespace();

  let TypedHeader {
    name,
    generics,
    extends,
    implements
  } = get_typed_header(false, sp)?;
  let extends = match extends.len() {
    len if len > 1 => return Err(CompilerError::new(
      class_start.value,
      "Classes can only extend once!".to_owned(),
    )),
    1 => Some(extends.last().unwrap().clone()),
    _ => None
  };

  // Mark import
  if let Some(Type::Custom(name)) = &extends {
    sp.st.mark_used_string(name);
  }

  // Classes change the way things are parsed!
  let mut kv_maps = SmallVec::new();
  let mut members: SmallVec<ClassMember> = SmallVec::new();

  sp.tokens.ignore_whitespace();
  sp.tokens.skip("{")?; // Skip body "{"
  sp.tokens.ignore_whitespace();

  while sp.tokens.peek_str() != "}" {
    // Note: here, "property" refers to anything defined with "=", which is
    // not inherently a function. "method" refers to things defined with
    // parenthesis, making them *always* be a function (or its declaration)

    // Get modifiers (if any)
    let modifiers = fetch_modifier_list(sp);

    // Get the name (might be a property or a method, we don't know yet)

    if sp.tokens.peek_str() == "[" {
      let init_token = sp.tokens.peek().clone();
      match parse_object_square_bracket(sp)? {
        ObjectSquareBracketReturn::KVMap(kv_map) => {
          kv_maps.push(kv_map);
        }
        ObjectSquareBracketReturn::ComputedProp(name) => {
          let (typ, value) = get_declaration_after_name(sp)?;
          members.push(ClassMember::Property(
            DeclarationComputable {
              name: ComputableDeclarationName::Computed(name),
              typ: typ,
              value
            },
            modifiers.clone()
          ));
        }
        ObjectSquareBracketReturn::MappedType(..) => {
          return Err(CompilerError::new(
            init_token.value,
            "Mapped types are not allowed in class bodies".to_owned(),
          ))
        }
      }
      
      sp.tokens.ignore_whitespace();

      // Skip ";" (if any)
      let _ = sp.tokens.try_skip_and_ignore_whitespace(";");
      continue;
    }

    // Check for getter/setter
    let checkpoint = sp.tokens.get_checkpoint();
    let mut is_getter = sp.tokens.peek_str() == "get";
    let mut is_setter = sp.tokens.peek_str() == "set";
    if is_getter || is_setter {
      sp.tokens.skip_unchecked();
      sp.tokens.ignore_whitespace();
      if IGNORE_MODIFIER_IF_SYMBOL.contains(&sp.tokens.peek_str()) {
        sp.tokens.restore_checkpoint(checkpoint);
        is_getter = false;
        is_setter = false;
      } else {
        sp.tokens.ignore_checkpoint(checkpoint);
      }
    } else {
      sp.tokens.ignore_checkpoint(checkpoint);
    }

    if sp.tokens.peek_str() == "{" && modifiers.has(Modifier::Static) {
      // Static initialization block
      sp.tokens.skip_unchecked(); // Skip "{"
      let body = get_block(sp)?;
      members.push(ClassMember::StaticBlock(body));
      sp.tokens.ignore_whitespace();
      let _ = sp.tokens.try_skip_and_ignore_whitespace(";");
      continue;
    }

    let name = sp.tokens.consume_type(TokenType::Identifier)?;
    sp.tokens.ignore_whitespace();

    if sp.str_src(name.value) != "constructor" && [ ":", "?", "!", "=", ";" ].contains(&sp.tokens.peek_str()) {
      // Normal property

      if is_getter || is_setter {
        return Err(CompilerError::new(
          sp.tokens.peek().value,
          "Getters and setters must be followed by a method body!".to_owned(),
        ));
      }

      // Get the declaration
      let (typ, value) = get_declaration_after_name(sp)?;
      members.push(ClassMember::Property(
        DeclarationComputable::named(name.value.to_owned(), typ, value),
        modifiers
      ));
    } else {
      // Otherwise, it's a method
      let mut function = if sp.str_src(name.value) == "constructor" {
        if is_getter || is_setter {
          return Err(CompilerError::new(
            name.value,
            "Constructors cannot be getters or setters!".to_owned(),
          ));
        }
        get_constructor_after_name(
          &mut members,
          sp,
        )?
      } else {
        get_function_after_name(
          Some(name.value),
          sp
        )?
      };

      function.modifiers.flags |= modifiers.flags;
      if !function.modifiers.has_accessibility() {
        function.modifiers.set(Modifier::Public);
      }
      members.push(ClassMember::Method(
        function,
        if is_getter {
          GetterSetter::Getter
        } else if is_setter {
          GetterSetter::Setter
        } else {
          GetterSetter::None
        }
      ));
    }

    sp.tokens.ignore_whitespace();

    // Skip ";" (if any)
    if sp.tokens.peek_str() == ";" {
      sp.tokens.skip_unchecked();
    }

    sp.tokens.ignore_whitespace();
  }
  sp.tokens.skip("}")?; // Skip body "}"

  Ok(sp.nodes.add(ASTNode::ClassDefinition { inner: Box::new(ClassDefinition {
    modifiers: ModifierList::new(),
    name,
    generics,
    extends,
    implements,
    kv_maps,
    members
  }) }))
}

struct TypedHeader {
  name: Option<SrcMapping>,
  generics: SmallVec<Type>,
  extends: SmallVec<Type>,
  implements: SmallVec<Type>,
}

/// Gets a typed header for a class or interface.
fn get_typed_header(
  require_name: bool,
  sp: &mut SourceProperties,
) -> Result<TypedHeader, CompilerError> {
  sp.tokens.ignore_whitespace();
  let name = sp.tokens.peek();
  let is_illegal_name = DISALLOWED_VARIABLE_NAMES.contains(sp.str_src(name.value));
  let name: Option<SrcMapping> = if name.typ == TokenType::Identifier && !is_illegal_name {
    Some(sp.tokens.consume().value)
  } else if require_name {
    let t = sp.tokens.consume();
    return Err(CompilerError::new(
      t.value,
      "Expected header name".to_owned(),
    ));
  } else {
    None
  };

  let generics = get_optional_generics(sp)?;
  sp.tokens.ignore_whitespace();

  let extends = if sp.tokens.peek_str() == "extends" {
    sp.tokens.skip_unchecked(); // Skip "extends"
    get_comma_separated_types_until(&[ "implements", "{", "=" ], sp)?
  } else {
    SmallVec::new()
  };
  sp.tokens.ignore_whitespace();

  let implements = if sp.tokens.peek_str() == "implements" {
    sp.tokens.skip_unchecked(); // Skip "implements"
    get_comma_separated_types_until(&[ "{", "=" ], sp)?
  } else {
    SmallVec::new()
  };

  Ok(TypedHeader {
    name, generics, extends, implements
  })
}

fn handle_modifiers(
  sp: &mut SourceProperties,
) -> Result<Option<ASTIndex>, CompilerError> {
  if !MODIFIERS.contains(&sp.tokens.peek_str()) {
    return Ok(None);
  }

  // Get the modifiers
  let modifiers = fetch_modifier_list(sp);
  if modifiers.is_empty() {
    return Err(CompilerError::new(
      sp.tokens.consume().value,
      "Token not allowed after modifiers!".to_owned(),
    ));
  }

  if modifiers.flags == (Modifier::Export as u8) && sp.tokens.peek_str() == "{" {
    // block export: export { ... }
    sp.tokens.skip_unchecked(); // Skip "{"
    sp.tokens.ignore_whitespace();

    let mut specifiers = SmallVec::new();

    while !sp.tokens.is_done() && sp.tokens.peek_str() != "}" {
      // Check for "type" keyword
      let is_type = sp.tokens.peek_str() == "type";
      if is_type {
        sp.tokens.skip_unchecked();
        sp.tokens.ignore_whitespace();
      }

      // Get the export name
      let name = sp.tokens.consume_type(TokenType::Identifier)?.value.to_owned();
      sp.tokens.ignore_whitespace();

      // Check for alias (as)
      let alias = if sp.tokens.peek_str() == "as" {
        sp.tokens.skip_unchecked();
        sp.tokens.ignore_whitespace();
        Some(sp.tokens.consume_type(TokenType::Identifier)?.value.to_owned())
      } else {
        None
      };
      sp.tokens.ignore_whitespace();

      specifiers.push(crate::ast::ExportSpecifier {
        name,
        alias,
        is_type,
      });

      // Skip comma if present
      if sp.tokens.peek_str() == "," {
        sp.tokens.skip_unchecked();
        sp.tokens.ignore_whitespace();
      }
    }

    sp.tokens.skip("}")?;

    Ok(Some(sp.nodes.add(ASTNode::StatementExport { 
      inner: Box::new(crate::ast::ExportDeclaration { specifiers }) 
    })))
  } else {
    // Get the node that goes after the modifiers
    let node_after_modifiers = get_single_statement(sp)?;

    sp.nodes.get_mut(node_after_modifiers).apply_modifiers(modifiers)?;
    Ok(Some(node_after_modifiers))
  }

}

fn fetch_modifier_list(sp: &mut SourceProperties) -> ModifierList {
  let mut modifiers: ModifierList = ModifierList::new();
  while MODIFIERS.contains(&sp.tokens.peek_str()) {
    let checkpoint = sp.tokens.get_checkpoint();
    let token = sp.tokens.consume();
    sp.tokens.ignore_whitespace();
    if IGNORE_MODIFIER_IF_SYMBOL.contains(&sp.tokens.peek_str()) {
      sp.tokens.restore_checkpoint(checkpoint);
      break;
    } else {
      sp.tokens.ignore_checkpoint(checkpoint);
    }
    modifiers.set(match sp.str_src(token.value) {
      "export" => Modifier::Export,
      "async" => Modifier::Async,
      "static" => Modifier::Static,
      "public" => Modifier::Public,
      "private" => Modifier::Private,
      "protected" => Modifier::Protected,
      "readonly" => Modifier::Readonly,
      "abstract" => Modifier::Abstract,
      "override" => Modifier::Override,
      other => panic!("Modifier not implemented: {:?}", other)
    });
  }
  modifiers
}

fn handle_type_declaration(
  sp: &mut SourceProperties,
) -> Result<Option<ASTIndex>, CompilerError> {
  if sp.tokens.peek_str() != "type" {
    return Ok(None);
  }
  let type_start = sp.tokens.consume();

  let TypedHeader {
    name, generics,
    extends, implements
  } = get_typed_header(true, sp)?;
  let name = unsafe { name.unwrap_unchecked() };
  if !extends.is_empty() {
    return Err(CompilerError::new(
      type_start.value,
      "Type declarations can't extend!".to_owned(),
    ))
  }
  if !implements.is_empty() {
    return Err(CompilerError::new(
      type_start.value,
      "Type declarations can't implement!".to_owned(),
    ))
  }

  // Consume `=`
  sp.tokens.ignore_whitespace();
  sp.tokens.skip("=")?;

  // Get the second type
  let equals_typ = get_type(sp)?;

  Ok(Some(sp.nodes.add(ASTNode::InterfaceDeclaration { inner: Box::new(InterfaceDeclaration {
    modifiers: ModifierList::new(),
    name,
    generics,
    extends,
    equals_type: equals_typ
  }) })))
}

fn handle_enum(
  is_const: bool,
  sp: &mut SourceProperties
) -> Result<Option<ASTIndex>, CompilerError> {
  if sp.tokens.peek_str() != "enum" {
    return Ok(None)
  }
  sp.tokens.skip_unchecked();
  sp.tokens.ignore_whitespace();

  let name = match sp.tokens.consume() {
    token if token.value.is_empty() => {
      return Err(CompilerError::new(
        token.value,
        "Expected enum name".to_owned(),
      ))
    },
    token => token.value.to_owned()
  };

  sp.tokens.ignore_whitespace();
  sp.tokens.skip("{")?;

  let mut counter: u32 = 0;
  let mut members: SmallVec<(SrcMapping, ASTIndex)> = SmallVec::new();

  while !sp.tokens.is_done() {
    sp.tokens.ignore_whitespace();
    let next = sp.tokens.peek_str();
    if next == "}" { break }

    let token = sp.tokens.consume();
    let name = match token.typ {
      TokenType::String => token.value.to_owned(),
      TokenType::Identifier => sp.add_custom_string(&format!("\"{}\"", sp.str_src(token.value))),
      _ => {
        return Err(CompilerError::new(
          token.value,
          "Expected string or identifier".to_owned(),
        ))
      }
    };
    sp.tokens.ignore_whitespace();
    let value = if sp.tokens.peek_str() == "=" {
      sp.tokens.skip_unchecked();
      get_expression(1, sp)? // TODO: why 1?
    } else {
      let number = counter.to_string();
      counter += 1;
      let number = sp.add_custom_string(&number);
      sp.nodes.add(ASTNode::ExprNumLiteral { number })
    };
    members.push((name, value));

    sp.tokens.ignore_whitespace();
    sp.tokens.ignore_commas();
  }

  sp.tokens.skip("}")?;

  Ok(Some(sp.nodes.add(ASTNode::EnumDeclaration { inner: Box::new(EnumDeclaration {
    modifiers: ModifierList::new(),
    name,
    members,
    is_const,
  }) })))
}

fn handle_interface(
  sp: &mut SourceProperties,
) -> Result<Option<ASTIndex>, CompilerError> {
  if sp.tokens.peek_str() != "interface" {
    return Ok(None);
  }
  let interface_start = sp.tokens.consume();

  let TypedHeader {
    name, generics,
    extends, implements
  } = get_typed_header(true, sp)?;
  let name = unsafe { name.unwrap_unchecked() };
  if !implements.is_empty() {
    return Err(CompilerError::new(
      interface_start.value,
      "Interfaces can't implement, only extend!".to_owned(),
    ))
  }

  sp.tokens.skip("{")?;

  // Store the parts of the interface!
  // Stores the function types in a multi-function interface
  let mut function_types = Type::Union(SmallVec::new());
  // Named key-value types
  let mut named_parts = SmallVec::new();
  // [key: type]
  let mut key_value = SmallVec::new();

  while !sp.tokens.is_done() {
    sp.tokens.ignore_whitespace();
    let next = sp.tokens.peek();
    let next_str = sp.str_src(next.value);
    if next_str == "}" { break }

    if next_str == "(" {
      // Function
      let function = get_type(sp)?;
      function_types.union(function);
    } else if next_str == "[" {
      // This could be either a Key-value map,
      // or a computed property
      match parse_object_square_bracket(sp)? {
        ObjectSquareBracketReturn::KVMap(kv_map) => key_value.push(kv_map),
        ObjectSquareBracketReturn::ComputedProp(value) => {
          named_parts.push(DeclarationTyped::computed(
            value,
            try_get_type(sp)?.unwrap_or(Type::Unknown)
          ))
        },
        ObjectSquareBracketReturn::MappedType(..) => {
          return Err(CompilerError::new(
            next.value,
            "Mapped types are not allowed in interface bodies".to_owned(),
          ))
        }
      }
    } else {
      // Try getting a named function
      let checkpoint = sp.tokens.get_checkpoint();
      let function_name = sp.tokens.consume();
      sp.tokens.ignore_whitespace();
      // TODO: make this get a typed declaration directly (without using a type in-between)
      if sp.tokens.peek_str() == "(" {
        // It's a function! (which is a member)
        sp.tokens.ignore_checkpoint(checkpoint);
        let function = get_type(sp)?;
        named_parts.push(DeclarationTyped::named(
          function_name.value.to_owned(),
          function
        ));
      } else {
        // It isn't a function, treat it as a named declaration
        sp.tokens.restore_checkpoint(checkpoint);
        let decl = get_declaration(sp)?;
        named_parts.push(DeclarationTyped::named(
          decl.name,
          decl.typ().clone()
        ));
      }
    }

    sp.tokens.ignore_whitespace();
    while [ ";", "," ].contains(&sp.tokens.peek_str()) {
      sp.tokens.skip_unchecked();
      sp.tokens.ignore_whitespace();
    }
  }

  sp.tokens.skip("}")?;

  let named_dict = Type::Object {
    key_value,
    parts: named_parts
  };

  let mut equals_type = Type::Intersection(SmallVec::new());
  if function_types.inner_count() != 0 { equals_type.intersection(function_types); }
  if named_dict.inner_count() != 0 { equals_type.intersection(named_dict); }
  Ok(Some(sp.nodes.add(ASTNode::InterfaceDeclaration { inner: Box::new(InterfaceDeclaration {
    modifiers: ModifierList::new(),
    name,
    generics,
    extends,
    equals_type: if equals_type.inner_count() == 0 {
      Type::Object { key_value: SmallVec::new(), parts: SmallVec::new() }
    } else if equals_type.inner_count() == 1 {
      match equals_type {
        Type::Intersection(inner) => inner[0].clone(),
        _ => panic!("")
      }
    } else {
      equals_type
    }
  }) })))
}

fn handle_try_catch(
  sp: &mut SourceProperties,
) -> Result<Option<ASTIndex>, CompilerError> {
  if sp.tokens.peek_str() != "try" {
    return Ok(None);
  }

  let try_token = sp.tokens.consume();
  sp.tokens.ignore_whitespace();
  let block_try = match handle_block(sp) {
    Ok(Some(block)) => block,
    _ => {
      return Err(CompilerError::new(
        try_token.value,
        "Expected block after `try`".to_owned(),
      ));
    }
  };

  sp.tokens.ignore_whitespace();
  let (capture_catch, capture_catch_type, block_catch) = if sp.tokens.peek_str() == "catch" {
    let token = sp.tokens.consume();
    sp.tokens.ignore_whitespace();
    let (capture_catch, capture_catch_type) = if sp.tokens.peek_str() == "(" {
      sp.tokens.skip_unchecked();
      sp.tokens.ignore_whitespace();
      let capture_catch = sp.tokens.consume_type(TokenType::Identifier)?.value.to_owned();
      sp.tokens.ignore_whitespace();
      let capture_catch_type = try_get_type(sp)?;
      sp.tokens.skip_with_whitespace(")")?;
      (Some(capture_catch), capture_catch_type)
    } else {
      (None, None)
    };
    let block_catch = match handle_block(sp) {
      Ok(Some(block)) => Some(block),
      _ => {
        return Err(CompilerError::new(
          token.value,
          "Expected block after `catch`".to_owned(),
        ));
      }
    };
    (capture_catch, capture_catch_type, block_catch)
  } else { (None, None, None) };
  
  sp.tokens.ignore_whitespace();
  let block_finally = if sp.tokens.peek_str() == "finally" {
    let token = sp.tokens.consume();
    sp.tokens.ignore_whitespace();
    match handle_block(sp) {
      Ok(Some(block)) => Some(block),
      _ => {
        return Err(CompilerError::new(
          token.value,
          "Expected block after `finally`".to_owned(),
        ))
      }
    }
  } else { None };

  if block_catch.is_none() && block_finally.is_none() {
    return Err(CompilerError::new(
      sp.tokens.peek().value,
      "Expected `catch` or `finally`".to_owned(),
    ))
  }

  return Ok(Some(sp.nodes.add(ASTNode::StatementTryCatchFinally {
    inner: Box::new(TryCatchFinally {
      block_try,
      capture_catch,
      capture_catch_type,
      block_catch,
      block_finally
    })
  })));
}

fn handle_declare(
  sp: &mut SourceProperties,
) -> Result<Option<ASTIndex>, CompilerError> {
  if sp.tokens.peek_str() != "declare" {
    return Ok(None);
  }
  sp.tokens.skip_unchecked();
  sp.tokens.ignore_whitespace();

  // TODO: return proper declarations instead of ignoring!
  while sp.tokens.peek_str() != "{" {
    sp.tokens.skip_unchecked();
  }
  sp.tokens.skip_unchecked();
  let _discarded_block = get_block(sp);

  Ok(Some(sp.nodes.add(ASTNode::Empty)))
}

/// Handles expressions. Basically a soft wrapper around `get_expression`
fn handle_expression(
  sp: &mut SourceProperties,
) -> Result<ASTIndex, CompilerError> {
  get_expression(0, sp)
}

fn parse_number(
  sp: &mut SourceProperties,
) -> Result<ASTIndex, CompilerError> {
  let mut number = sp.tokens.consume().value.to_owned();
  let p = sp.tokens.peek();
  if p.typ == TokenType::Identifier {
    match sp.str_src(p.value).chars().next().unwrap() {
      'x' | 'b' | 'o' => {
        number.len += p.value.len;
        sp.tokens.skip_unchecked();
      }
      _ => {
        return Err(CompilerError::new(
          p.value,
          "Unknown numeric type".to_owned(),
        ))
      }
    }
  }
  Ok(sp.nodes.add(ASTNode::ExprNumLiteral { number }))
}

fn parse_string(
  sp: &mut SourceProperties,
) -> ASTIndex {
  sp.nodes.add(ASTNode::ExprStrLiteral {
    string: sp.tokens.consume().value
  })
}

fn parse_string_template(
  sp: &mut SourceProperties,
) -> Result<ASTIndex, CompilerError> {
  let head_token = sp.tokens.consume();
  if head_token.typ == TokenType::String {
    return Ok(sp.nodes.add(ASTNode::ExprStrLiteral {
      string: head_token.value
    }));
  }

  let mut head = head_token.value;
  head.idx += 1;
  head.len -= 3;

  let mut parts = SmallVec::new();

  while sp.tokens.peek().typ != TokenType::EndOfFile {
    let expr = get_expression(0, sp)?;
    let literal_part = sp.tokens.consume();
    if literal_part.typ != TokenType::StringTemplateMiddle && literal_part.typ != TokenType::StringTemplateEnd {
      return Err(CompilerError::new(
        literal_part.value,
        "Expected string template part".to_owned(),
      ));
    }
    let first_char_size = sp.str_src(literal_part.value).chars().next().unwrap().len_utf8() as u32;
    parts.push((expr, match literal_part.typ {
      TokenType::StringTemplateMiddle => {
        SrcMapping {
          idx: literal_part.value.idx + first_char_size,
          len: literal_part.value.len - first_char_size - 2,
          from: literal_part.value.from,
        }
      }
      TokenType::StringTemplateEnd => {
        SrcMapping {
          idx: literal_part.value.idx + first_char_size,
          len: literal_part.value.len - first_char_size - 1,
          from: literal_part.value.from,
        }
      }
      _ => unreachable!()
    }));
    if literal_part.typ == TokenType::StringTemplateEnd { break; }
    if sp.tokens.peek().typ == TokenType::EndOfFile {
      return Err(CompilerError::new(
        sp.tokens.peek().value,
        "Unterminated template literal".to_owned(),
      ));
    }
  }

  Ok(sp.nodes.add(ASTNode::ExprTemplateLiteral {
    inner: Box::new(ExprTemplateLiteral { head, parts })
  }))
}

fn parse_regex(
  sp: &mut SourceProperties,
) -> ASTIndex {
  todo!("Reimplement with new pooling...");
  // let val = sp.tokens.consume().value;

  // let (pattern, flags) = val[1..].split_once('/')
  //   .unwrap_or((&val[1..], ""));

  // sp.arena.add(ASTNode::ExprRegexLiteral { inner: Box::new(ExprRegexLiteral {
  //   pattern: pattern.to_string(), 
  //   flags: flags.to_string() 
  // }) })
}

fn parse_name(
  sp: &mut SourceProperties,
) -> Result<ASTIndex, CompilerError> {
  let token = sp.tokens.consume();
  match sp.str_src(token.value) {
    "true" => { Ok(sp.nodes.add(ASTNode::ExprBoolLiteral { value: true })) }
    "false" => { Ok(sp.nodes.add(ASTNode::ExprBoolLiteral { value: false })) }
    name => {
      // Mark symbol as used (not in type context)
      sp.st.mark_used(&token, sp.source)?;
      Ok(sp.nodes.add(ASTNode::ExprIdentifier { name: token.value }))
    }
  }
}

fn parse_prefix(
  precedence: u8,
  sp: &mut SourceProperties,
) -> Result<ASTIndex, CompilerError> {
  let prefix_start = sp.tokens.consume().value;
  let prefix_start_str = sp.str_src(prefix_start);

  let is_grouping = [ "(", "[" ].contains(&prefix_start_str);
  if is_grouping {
    // Groupings get special treatment!
    let nodes = separate_commas(get_expression(0, sp)?, sp);
    let ret = match prefix_start_str {
      "(" => ASTNode::Parenthesis { nodes },
      "[" => ASTNode::Array { nodes },
      unknown_grouping => {
        panic!("Grouping not implemented: {}", unknown_grouping);
      }
    };
    let group_end = INVERSE_GROUPINGS[prefix_start_str];
    sp.tokens.skip(group_end)?; // Skip grouping close ")" or "]"
    Ok(sp.nodes.add(ret))
  } else if prefix_start_str == "{" {
    // Dicts get even more special treatment!
    let mut properties = SmallVec::new();
    loop {
      sp.tokens.ignore_whitespace();
      let key_token = sp.tokens.peek();
      if sp.str_src(key_token.value) == "}" { break; }

      if sp.str_src(key_token.value) == "..." {
        // Spread
        sp.tokens.skip_unchecked(); // Skip "..."

        properties.push(ObjectProperty::Rest {
          argument: get_expression(1, sp)?
        });
      } else {
        // Identifier, String, Number, or computed key
        let (computed, key) = match (&key_token.typ, sp.str_src(key_token.value)) {
          (TokenType::Identifier, _) => {
            (false, sp.nodes.add(ASTNode::ExprIdentifier { name: sp.tokens.consume().value }))
          },
          (TokenType::String, _) => {
            (false, sp.nodes.add(ASTNode::ExprStrLiteral { string: sp.tokens.consume().value }))
          },
          (TokenType::Number, _) => {
            (false, sp.nodes.add(ASTNode::ExprNumLiteral { number: sp.tokens.consume().value }))
          }
          (TokenType::Symbol, "[") => {
            // Computed key
            sp.tokens.skip_unchecked();
            let inner_key = get_expression(0, sp)?;
  
            sp.tokens.ignore_whitespace();
            sp.tokens.skip("]")?;
  
            (true, inner_key)
          },
          other => {
            return Err(CompilerError::new(
              key_token.value,
              format!("Expected property assignment, found {:?}", other),
            ));
          }
        };
  
        // Skip ":"
        sp.tokens.ignore_whitespace();
        if let ASTNode::ExprIdentifier { name } = sp.nodes.get(key) {
          if sp.tokens.peek_str() != ":" {
            properties.push(ObjectProperty::Shorthand { key: name.clone() });
            sp.tokens.ignore_commas();
            continue;
          }
          sp.tokens.skip(":")?;
        } else {
          sp.tokens.skip(":")?;
        }
  
        // Get value
        let value = get_expression(1, sp)?;
        
        properties.push(ObjectProperty::Property {
          computed,
          key,
          value
        });
      }

      // Ignore trailing commas (if any)
      sp.tokens.ignore_commas();
    }
    sp.tokens.skip("}")?;

    Ok(sp.nodes.add(ASTNode::Dict { properties }))
  } else {
    let expr = get_expression(precedence, sp)?;
    Ok(sp.nodes.add(ASTNode::PrefixOpr {
      opr: prefix_start, expr
    }))
  }
}

/// Parses an infix operator. Ran when the current token is known to be infix.
fn parse_infix<'a, 'b>(
  left: ASTIndex,
  precedence: u8,
  sp: &mut SourceProperties,
) -> Result<ASTIndex, CompilerError> where 'a: 'b {

  if let ASTNode::ExprIdentifier { name } = sp.nodes.get(left) {
    if sp.str_src(*name) == "async" && sp.tokens.peek_str() == "(" {
      let arrow_fn = parse_arrow_function(sp)?;
      match sp.nodes.get_mut(arrow_fn) {
        ASTNode::ArrowFunctionDefinition { inner } => {
          inner.is_async = true;
        }
        _ => unreachable!()
      }
      return Ok(arrow_fn);
    }
  }

  let opr_token = sp.tokens.consume();
  let opr = opr_token.value;
  let opr_str = sp.str_src(opr);

  match opr_str {
    "(" | "[" => {
      // Function call or property access (indexing)

      // Get children
      let inner = get_expression(0, sp)?;
      let group_end = INVERSE_GROUPINGS[opr_str];
      sp.tokens.skip(group_end)?; // Skip trailing group

      match opr_str {
        "(" => {
          if let ASTNode::ExprIdentifier { name } = sp.nodes.get(left) {
            if sp.str_src(*name) == "import" {
              // Dynamic import
              return Ok(sp.nodes.add(ASTNode::ExpressionImport {
                value: inner
              }));
            }
          }
          let arguments = separate_commas(inner, sp);
          return Ok(sp.nodes.add(ASTNode::ExprFunctionCall {
            inner: Box::new(ExprFunctionCall {
              callee: left,
              generics: SmallVec::new(),
              arguments
            })
          }))
        },
        "[" => return Ok(sp.nodes.add(ASTNode::ExprIndexing {
          callee: left,
          property: inner
        })),
        other => { panic!("Grouping not implemented: {}", other); }
      };
    }
    "=>" => {
      // Arrow functions get special treatment!
      // This captures arrow functions that don't start with parenthesis, or those
      // which don't have an arrow-function-specific header.

      // This avoids re-parsing stuff that can be salvaged!

      let params = match sp.nodes.get(left) {
        ASTNode::ExprIdentifier { name } => {
          SmallVec::with_element(DestructurableDeclaration {
            name: DestructurePattern::Identifier { name: name.clone() },
            typ: Type::Unknown
          })
        }
        ASTNode::Parenthesis { nodes } => {
          let mut params = SmallVec::new();
          for n in nodes {
            params.push(match sp.nodes.get(*n) {
              ASTNode::ExprIdentifier { name } => DestructurableDeclaration {
                name: DestructurePattern::Identifier { name: name.clone() },
                typ: Type::Unknown
              },
              ASTNode::InfixOpr {
                left_right, opr, ..
              } if sp.str_src(*opr) == "=" && matches!(sp.nodes.get(left_right.0), ASTNode::ExprIdentifier { .. }) => match sp.nodes.get(left_right.0) {
                ASTNode::ExprIdentifier { name } => DestructurableDeclaration {
                  name: DestructurePattern::Identifier { name: name.clone() },
                  typ: Type::Unknown
                },
                _ => unreachable!()
              }
              ASTNode::Empty => { continue; }
              other => return Err(CompilerError::new(
                sp.nodes.get_node_src_range(*n),
                format!("Arrow function expected parameter, found {:?}", other),
              ))
            });
          }
          params
        }
        other => {
          return Err(CompilerError::new(
            sp.nodes.get_node_src_range(left),
            format!(
              "Arrow function expected parenthesis or identifier, found {:?}",
              other
            ),
          ))
        }
      };

      return Ok(parse_arrow_function_after_arrow(
        params,
        Rest::new(),
        Type::Unknown,
        sp
      )?);
    }
    "?" => {
      // Ternary (eg. `condition ? true : false`)

      // Get the first part, up until the `:`
      let mut if_true = get_expression(*crate::operations::COLON_PRECEDENCE, sp)?;
      while sp.tokens.peek_str() != ":" {
        if sp.tokens.is_done() {
          return Err(CompilerError::new(
            sp.tokens.consume().value,
            "Ternary not closed!".to_string(),
          ));
        }
        if_true = parse_infix(
          if_true,
          *crate::operations::COLON_PRECEDENCE,
          sp
        )?;
        sp.tokens.ignore_whitespace();
      }
      sp.tokens.skip_unchecked();
      let if_false = get_expression(precedence, sp)?;

      return Ok(sp.nodes.add(ASTNode::ExprTernary {
        condition: left,
        if_true, if_false,
      }));
    }
    "<" => {
      // An inline "<" could potentially be a generic argument!
      let checkpoint = sp.tokens.get_checkpoint();

      // Try parsing generics...
      if let Ok(mut generics) = get_generics(sp) {
        if sp.tokens.peek_str() == "(" {
          sp.tokens.ignore_checkpoint(checkpoint);
          let fn_call = parse_infix(left, precedence, sp)?;
          if let ASTNode::ExprFunctionCall { inner } = sp.nodes.get_mut(fn_call) {
            inner.generics.append(&mut generics);
          }
          return Ok(fn_call)
        }
      }

      // Since this didn't work, go back
      sp.tokens.restore_checkpoint(checkpoint);
    }
    _ => {}
  }

  // Normal infix
  let right = get_expression(precedence, sp)?;
  Ok(sp.nodes.add(ASTNode::InfixOpr {
    left_right: (left, right),
    opr,
  }))
}

/// Turns a tree containing commas into a vector with the nodes they separated
fn separate_commas(
  node: ASTIndex,
  sp: &mut SourceProperties,
) -> SmallVec<ASTIndex> {
  let sp_inner = sp as *const SourceProperties;
  let left_right: (ASTIndex, ASTIndex) = match sp.nodes.get_mut(node) {
    // Separate infix operations (but only if the operation is ",")
    ASTNode::InfixOpr { left_right, opr } if (unsafe {
      (&*sp_inner).str_src(*opr)
    }) == "," => {
      // The size of the output vec is calculated from the sum
      // of both sides, so there's no wasted capacity.
      *left_right
    }

    // Otherwise, return the node, as it's between commas
    _ => { return SmallVec::with_element(node); }
  };

  let mut nodes = separate_commas(left_right.0, sp);
  nodes.append(&mut separate_commas(left_right.1, sp));
  nodes
}

/// Tries parsing an arrow function, returning CompilerError if none is present.
/// Caller is responsible for restoring the tokenizer back to its origial state
/// if this function doesn't return successfully.
fn parse_arrow_function(
  sp: &mut SourceProperties,
) -> Result<ASTIndex, CompilerError> {
  sp.tokens.skip("(")?;
  let (params, spread) = get_multiple_destructurable_declarations(true, sp)?;
  sp.tokens.skip(")")?;
  sp.tokens.ignore_whitespace();
  let return_type = try_get_type(sp)?;
  sp.tokens.skip("=>")?;
  Ok(parse_arrow_function_after_arrow(
    params,
    spread,
    return_type.unwrap_or(Type::Unknown),
    sp
  )?)
}

/// Parses an arrow function starting at the arrow (`=>`)
fn parse_arrow_function_after_arrow(
  params: SmallVec<DestructurableDeclaration>,
  rest: Rest,
  return_type: Type,
  sp: &mut SourceProperties,
) -> Result<ASTIndex, CompilerError> {
  sp.tokens.ignore_whitespace();
  let is_expression = sp.tokens.peek_str() != "{";
  let body = if is_expression {
    get_expression(*crate::operations::ARROW_FN_PRECEDENCE, sp)?
  } else {
    sp.tokens.skip_unchecked(); // Skip "{"
    get_block(sp)?
  };

  Ok(sp.nodes.add(ASTNode::ArrowFunctionDefinition { inner: Box::new(ArrowFunctionDefinition {
    is_async: false,
    generics: SmallVec::new(),
    params,
    rest,
    return_type,
    body
  }) }))
}

/// Gets a single expression
pub fn get_expression<'a, 'b>(
  precedence: u8,
  sp: &mut SourceProperties,
) -> Result<ASTIndex, CompilerError> where 'a: 'b {
  sp.tokens.ignore_whitespace();

  // Get left (or sometimes only) side (which can be the prexfix!)
  let mut left = {
    let next = sp.tokens.peek();

    if [ ")", "]", "}", ",", ";" ].contains(&sp.str_src(next.value)) || next.typ == TokenType::EndOfFile {
      // End it here!
      return Ok(sp.nodes.add(ASTNode::Empty));
    }

    match next.typ {
      TokenType::Number => parse_number(sp)?,
      TokenType::String => parse_string(sp),
      TokenType::Regex => parse_regex(sp),
      TokenType::StringTemplateStart => parse_string_template(sp)?,
      TokenType::Symbol | TokenType::Identifier => {
        let next_str = sp.str_src(next.value);
        if next_str == "class" {
          // `class` can be used inside an expression, but calling it a
          // prefix feels strange... I'm going to handle it here
          get_class_expression(sp)?
        } else if next_str == "function" {
          sp.tokens.skip_unchecked(); // Skip "function"
          let function = get_function_after_name(None, sp)?;
          sp.nodes.add(ASTNode::FunctionDefinition { inner: Box::new(function) })
        } else {
          let binding_power = get_operator_binding_power(
            ExprType::Prefx,
            next_str
          );
          if let Some(binding_power) = binding_power {
            // These prefix operators include what you might expect (+, -, ~),
            // along with arrays, dicts, and parenthesis!
            if next_str == "(" {
              // A parenthesis could be either a normal expression, or an arrow
              // function. Since normal expressions are more common,
              // we try parsing those first.

              // Check if it ends with a type!
              let checkpoint = sp.tokens.get_checkpoint();
              let mut paren_nesting = 1;
              sp.tokens.skip_unchecked(); // Skip `(`
              while paren_nesting != 0 {
                let token = sp.tokens.consume();
                match sp.str_src(token.value) {
                  "(" => paren_nesting += 1,
                  ")" => paren_nesting -= 1,
                  _ => {}
                }
              }
              sp.tokens.ignore_whitespace();
              let is_arrow_function = ["=>", ":"].contains(&sp.tokens.peek_str()) && precedence <= *ARROW_FN_PRECEDENCE;
              sp.tokens.restore_checkpoint(checkpoint);

              if is_arrow_function {
                parse_arrow_function(sp)?
              } else {
                parse_prefix(binding_power.1, sp)?
              }
            } else {
              parse_prefix(binding_power.1, sp)?
            }
          } else if next.typ == TokenType::Identifier {
            // Could be a name...
            parse_name(sp)?
          } else if next_str == "<" {
            // Arrow function generic, or C-style type cast (eg. `<number>a`)
            let generics_token = sp.tokens.peek().clone();
            sp.tokens.skip_unchecked();
            let generics = get_generics(sp)?;

            // Get next expression...
            let mut expr = get_expression(
              get_operator_binding_power(ExprType::Prefx, "<>").unwrap().0,
              sp
            )?;
            match sp.nodes.get_mut(expr) {
              ASTNode::ArrowFunctionDefinition { inner } => {
                inner.generics = generics;
              },
              _ => {
                if generics.len() > 1 {
                  return Err(CompilerError::new(
                    generics_token.value,
                    format!("Expected one type in cast, found {}", generics.len()),
                  ));
                }
                expr = sp.nodes.add(ASTNode::ExprTypeAssertion {
                  cast_type: Box::new(generics[0].clone()), value: expr
                });
              }
            }

            expr
          } else {
            // Not a name & no matching operators.
            let t = sp.tokens.peek().value;
            let opr_str = sp.str_src(t);
            return Err(CompilerError::new(
              sp.tokens.consume().value,
              "Prefix operator not found".to_owned(),
            ));
          }
        }
      },
      _ => {
        return Err(CompilerError::new(
          sp.tokens.consume().value,
          format!(
            "Unexpected token when parsing expression: {:?}",
            next
          ),
        ));
      },
    }
  };

  // If there's a next operator, parse it
  loop {
    // Get the next token
    sp.tokens.ignore_whitespace();
    let next = sp.tokens.peek();
    if next.typ == TokenType::EndOfFile { break; }
    let next_str = sp.str_src(next.value);

    // `as` statements get special treatment
    if next_str == "as" {
      sp.tokens.skip_unchecked();
      let cast_type = get_type(sp)?;
      left = sp.nodes.add(ASTNode::ExprAs {
        value: left,
        cast_type: Box::new(cast_type)
      });
      continue;
    }

    // Handle postfix
    if let Some(binding_power) = get_operator_binding_power(
      ExprType::Pstfx,
      next_str
    ) {
      if binding_power.0 < precedence {
        break;
      }
      left = if sp.tokens.peek_str() == "!" {
        // Non-null assertion
        sp.tokens.skip_unchecked(); // Skip "!"
        sp.nodes.add(ASTNode::NonNullAssertion { expr: left })
      } else {
        // Any other postfix operator
        sp.nodes.add(ASTNode::PostfixOpr {
          expr: left,
          opr: sp.tokens.consume().value
        })
      };
      continue;
    }

    // Handle template literal tags
    if next_str.starts_with("`") {
      if *TEMPLATE_LITERAL_TAG_PRECEDENCE < precedence {
        break;
      }
      let argument = parse_string_template(sp)?;
      left = sp.nodes.add(ASTNode::TemplateLiteralTag {
        callee: left,
        argument
      });
      continue;
    }

    // Handle infix
    let binding_power = if let Some(binding_power) = get_operator_binding_power(
      ExprType::Infx,
      next_str
    ) {
      if binding_power.0 < precedence {
        break;
      }
      binding_power
    } else {
      // If there is no infix for this symbol, stop
      break;
    };

    // It's infix
    left = parse_infix(left, binding_power.1, sp)?;
  }

  Ok(left)
}
