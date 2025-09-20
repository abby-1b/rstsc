use phf::{phf_map, phf_set};
use crate::{
  ast::{
    ASTNode, ArrowFunctionDefinition, ClassDefinition, ClassMember, EnumDeclaration, FunctionDefinition, GetterSetter, ImportDefinition, IndividualImport, InterfaceDeclaration, ObjectProperty
  }, ast_common::{
    Modifier, ModifierList, VariableDefType, ACCESSIBILITY_MODIFIERS, MODIFIERS
  }, declaration::{ComputableDeclarationName, Declaration, DeclarationComputable, DeclarationTyped, DestructurableDeclaration, DestructurePattern}, error_type::CompilerError, operations::{get_operator_binding_power, ExprType, ARROW_FN_PRECEDENCE, COMMA_PRECEDENCE}, rest::Rest, small_vec::SmallVec, tokenizer::{Token, TokenList, TokenType}, types::{
    get_comma_separated_types_until, get_generics, get_optional_generics, get_type, parse_object_square_bracket, try_get_type, ObjectSquareBracketReturn, Type
  }
};

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
pub fn get_block(tokens: &mut TokenList) -> Result<ASTNode, CompilerError> {
  let mut nodes = SmallVec::new();

  // Go through the tokens list
  loop {
    nodes.push(get_single_statement(tokens)?);

    if tokens.peek_str() == ";" { tokens.skip_unchecked(); continue; }
    if tokens.is_done() { break; }
    if [ ")", "]", "}" ].contains(&tokens.peek_str()) { tokens.skip_unchecked(); break; }
  }

  // Remove trailing empty
  if let Some(node) = nodes.last() {
    if matches!(node, ASTNode::Empty) {
      nodes.pop();
    }
  }

  // Return the program node
  Ok(ASTNode::Block { nodes })
}

/// Gets a single statement, until it reaches either a `;` or a group end
/// (eg. `)]}`). Only consumes semicolons, not group ends. If a group start is
/// reached (eg. `([{`), calls the corresponding function for said group.
fn get_single_statement<'a, 'b>(
  tokens: &'b mut TokenList
) -> Result<ASTNode, CompilerError> where 'a: 'b {
  // Ignore whitespace
  tokens.ignore_whitespace();
  
  // Handlers
  // Note that handlers don't consume `;`!
  let ret = if let Some(block) = handle_blocks(tokens)? { block }
  else if let Some(vars)                 = handle_vars(tokens)? { vars }
  else if let Some(control_flow)         = handle_control_flow(tokens)? { control_flow }
  else if let Some(function_declaration) = handle_function_declaration(tokens)? { function_declaration }
  else if let Some(class_declaration)    = handle_class_declaration(tokens)? { class_declaration }
  else if let Some(modifiers)            = handle_modifiers(tokens)? { modifiers }
  else if let Some(import)               = handle_import(tokens)? { import }
  else if let Some(other_statements)     = handle_other_statements(tokens)? { other_statements }
  else if let Some(type_declaration)     = handle_type_declaration(tokens)? { type_declaration }
  else if let Some(result_enum)          = handle_enum(tokens, false)? { result_enum }
  else if let Some(interface)            = handle_interface(tokens)? { interface }
  else { handle_expression(tokens)? }; // Expression fallback

  tokens.ignore_whitespace();
  if tokens.peek_str() == ";" {
    tokens.skip_unchecked();
  }

  Ok(ret)
}

/// Handles blocks (defined as a non-expression `{` token)
fn handle_blocks(
  tokens: &mut TokenList
) -> Result<Option<ASTNode>, CompilerError> {
  if tokens.peek_str() != "{" {
    return Ok(None);
  }
  tokens.skip_unchecked(); // Skip "{"
  Ok(Some(get_block(tokens)?))
}

/// Handles variable initialization
fn handle_vars<'a, 'b>(
  tokens: &'b mut TokenList
) -> Result<Option<ASTNode>, CompilerError> where 'a: 'b {
  if !VARIABLE_DECLARATIONS.contains(&tokens.peek_str()) {
    return Ok(None);
  }

  let def_type = get_variable_def_type(tokens)?;

  if matches!(def_type, VariableDefType::Const) && tokens.peek_str() == "enum" {
    // Const enums
    return handle_enum(tokens, true);
  }

  // Get the variable definitions
  let (defs, _) = get_multiple_destructurable_declarations(tokens, false)?;

  Ok(Some(ASTNode::VariableDeclaration {
    modifiers: ModifierList::new(),
    def_type,
    defs
  }))
}

fn get_variable_def_type<'b>(
  tokens: &mut TokenList<'b>
) -> Result<VariableDefType, CompilerError> {
  // Get the header
  let header_token = tokens.consume();
  tokens.ignore_whitespace();
  match header_token.value {
    "var"   => Ok(VariableDefType::Var),
    "let"   => Ok(VariableDefType::Let),
    "const" => Ok(VariableDefType::Const),
    other => Err(CompilerError::new(
      format!("Unexpected variable declaration: {}", other),
      header_token, tokens
    ))
  }
}

/// Gets a single named (and optionally typed) declaration,
/// with or without a value. Does NOT handle rest parameters!
fn get_declaration<'a, 'b>(
  tokens: &'b mut TokenList
) -> Result<Declaration, CompilerError> where 'a: 'b {
  tokens.ignore_whitespace();
  let name = tokens.consume_type(TokenType::Identifier)?.value.to_owned();
  let (typ, value) = get_declaration_after_name(tokens)?;
  Ok(Declaration::new(name, typ, value))
}

fn get_declaration_after_name<'a, 'b>(
  tokens: &'b mut TokenList,
) -> Result<(Type, Option<ASTNode>), CompilerError> where 'a: 'b {
  let typ = if tokens.try_skip_and_ignore_whitespace("?") {
    if tokens.peek_str() != ":" {
      return Err(CompilerError::new(
        "Expected `:` after `?` in conditional declaration".to_owned(),
        tokens.consume(), tokens
      ))
    }
    let mut typ = get_type(tokens)?;
    typ.intersection(Type::Void);
    Some(typ)
  } else if tokens.try_skip_and_ignore_whitespace("!") {
    if tokens.peek_str() != ":" {
      return Err(CompilerError::new(
        "Expected `:` after `!` in non-null asserted declaration".to_owned(),
        tokens.consume(), tokens
      ))
    }
    Some(get_type(tokens)?)
  } else {
    try_get_type(tokens)?
  }.unwrap_or(Type::Unknown);

  tokens.ignore_whitespace();
  let value = if tokens.peek_str() == "=" {
    tokens.skip_unchecked();
    Some(get_expression(tokens, *crate::operations::COMMA_PRECEDENCE)?)
  } else {
    None
  };

  Ok((typ, value))
}

fn get_destructurable_declaration<'a, 'b>(
  tokens: &'b mut TokenList
) -> Result<DestructurableDeclaration, CompilerError> where 'a: 'b {
  let name = parse_destructure_pattern(tokens, false)?;
  let (typ, initializer) = get_declaration_after_name(tokens)?;
  Ok(DestructurableDeclaration {
    name: if let Some(initializer) = initializer {
      DestructurePattern::WithInitializer { pattern: Box::new(name), initializer }
    } else { name },
    typ
  })
}

/// Gets multiple named declarations, with or without values.
/// Stops when it encounters a semicolon or closing parenthesis
/// 
/// Examples (in square brackets):
/// 
/// `let [a: number = 123, b: string];`
/// 
/// `function some([a: number, b: string = '123']) { ... }`
fn get_multiple_declarations<'a, 'b>(
  tokens: &'b mut TokenList,
  allow_rest: bool
) -> Result<(SmallVec<Declaration>, Rest), CompilerError> where 'a: 'b {
  tokens.ignore_whitespace();
  let mut declarations = SmallVec::new();
  let mut rest = Rest::new();
  while ![ ";", ")" ].contains(&tokens.peek_str()) {
    rest.try_set(tokens, allow_rest)?;
    declarations.push(get_declaration(tokens)?);
    if !tokens.ignore_commas() { break }
  }
  Ok((declarations, rest))
}

fn get_multiple_destructurable_declarations<'a, 'b>(
  tokens: &'b mut TokenList,
  allow_rest: bool
) -> Result<(SmallVec<DestructurableDeclaration>, Rest), CompilerError> where 'a: 'b {
  tokens.ignore_whitespace();
  let mut declarations: SmallVec<DestructurableDeclaration> = SmallVec::new();
  let mut rest = Rest::new();
  while ![ ";", ")" ].contains(&tokens.peek_str()) {
    rest.try_set(tokens, allow_rest)?;
    declarations.push(get_destructurable_declaration(tokens)?);
    if !tokens.ignore_commas() { break }
  }
  Ok((declarations, rest))
}

fn parse_destructure_pattern(
  tokens: &mut TokenList,
  capture_initializer: bool
) -> Result<DestructurePattern, CompilerError> {
  tokens.ignore_whitespace();
  let pattern = match tokens.peek_str() {
    "[" => {
      // Array destructure
      tokens.skip_unchecked(); // Skip "["
      let mut elements = SmallVec::new();
      let mut spread = None;
      while tokens.peek_str() != "]" {
        tokens.ignore_whitespace();
        if tokens.peek_str() == "..." {
          if spread.is_some() {
            return Err(CompilerError::new(
              "Only one spread allowed in array destructure".to_owned(),
              tokens.consume(), tokens
            ))
          }
          tokens.skip_unchecked();
          spread = Some(Box::new(parse_destructure_pattern(tokens, false)?));
        } else if tokens.peek_str() == "," {
          if spread.is_some() {
            return Err(CompilerError::new(
              "A spread may not have a trailing comma".to_owned(),
              tokens.consume(), tokens
            ))
          }
          while tokens.try_skip_and_ignore_whitespace(",") {
            elements.push(DestructurePattern::Ignore);
          }
        } else {
          if spread.is_some() {
            return Err(CompilerError::new(
              "A spread must be last in a destructuring pattern".to_owned(),
              tokens.consume(), tokens
            ))
          }
          elements.push(parse_destructure_pattern(tokens, true)?);
        }
        tokens.ignore_whitespace();
        if tokens.peek_str() == "," {
          tokens.skip_unchecked();
        }
      }
      tokens.skip_unchecked(); // Skip "]"
      tokens.ignore_whitespace();
      DestructurePattern::Array { elements, spread }
    },
    "{" => {
      // Object destructure
      tokens.skip_unchecked(); // Skip "{"
      let mut properties = SmallVec::new();
      let mut rest = None;
      while tokens.peek_str() != "}" {
        tokens.ignore_whitespace();
        if tokens.peek_str() == "..." {
          if rest.is_some() {
            return Err(CompilerError::new(
              "Only one rest element allowed in object destructure".to_owned(),
              tokens.consume(), tokens
            ))
          }
          tokens.skip_unchecked();
          rest = Some(Box::new(parse_destructure_pattern(tokens, false)?));
          if tokens.peek_str() == "=" {
            return Err(CompilerError::new(
              "A rest element cannot have an initializer".to_owned(),
              tokens.consume(), tokens
            ))
          }
        } else if tokens.peek_str() == "," {
          if rest.is_some() {
            return Err(CompilerError::new(
              "A rest element may not have a trailing comma".to_owned(),
              tokens.consume(), tokens
            ))
          }
          tokens.skip_unchecked();
        } else {
          if rest.is_some() {
            return Err(CompilerError::new(
              "A spread must be last in a destructuring pattern".to_owned(),
              tokens.consume(), tokens
            ))
          }
          let property = parse_destructure_pattern(tokens, false)?;
          let needs_alias = match property {
            DestructurePattern::NumericProperty { .. } => true,
            DestructurePattern::StringProperty { .. } => true,
            _ => false
          };
          let alias = if tokens.peek_str() == ":" {
            tokens.skip_unchecked();
            parse_destructure_pattern(tokens, false)?
          } else if needs_alias {
            return Err(CompilerError::new(
              "Expected `:`".to_owned(),
              tokens.consume(), tokens
            ));
          } else {
            property.clone()
          };
          let alias = try_parse_destructure_pattern_initializer(tokens, alias)?;
          tokens.ignore_whitespace();
          properties.push((property, alias));
        }
        tokens.ignore_whitespace();
        if tokens.peek_str() == "," {
          tokens.skip_unchecked();
        }
      }
      tokens.skip_unchecked(); // Skip "}"
      tokens.ignore_whitespace();
      DestructurePattern::Object { properties, spread: rest }
    },
    _ => {
      // Identifier, numeric property, or string property
      let token = tokens.consume();
      let out = match token.typ {
        TokenType::Identifier => DestructurePattern::Identifier { name: token.value.to_owned() },
        TokenType::Number => DestructurePattern::NumericProperty { value: token.value.to_owned() },
        TokenType::String => DestructurePattern::StringProperty { value: token.value.to_owned() },
        _ => return Err(CompilerError::new(
          "Expected Identifier, Number or String in destructure pattern".to_owned(),
          token, tokens
        ))
      };
      tokens.ignore_whitespace();
      out
    }
  };

  if capture_initializer {
    try_parse_destructure_pattern_initializer(tokens, pattern)
  } else {
    Ok(pattern)
  }
}

#[inline]
fn try_parse_destructure_pattern_initializer(
  tokens: &mut TokenList,
  curr_pattern: DestructurePattern
) -> Result<DestructurePattern, CompilerError> {
  if tokens.peek_str() == "=" {
    tokens.skip_unchecked();
    Ok(DestructurePattern::WithInitializer {
      pattern: Box::new(curr_pattern),
      initializer: get_expression(tokens, *COMMA_PRECEDENCE)?
    })
  } else {
    Ok(curr_pattern)
  }
}

/// Handles control flow, like `if`, `while`, and `for`
fn handle_control_flow(
  tokens: &mut TokenList
) -> Result<Option<ASTNode>, CompilerError> {
  const CONTROL_FLOW: &[&str] = &[ "if", "while", "for", "switch" ];
  if !CONTROL_FLOW.contains(&tokens.peek_str()) {
    return Ok(None);
  }

  let control_flow_type = tokens.consume().value.to_string();
  tokens.ignore_whitespace();

  Ok(Some(match control_flow_type.as_str() {
    "if" | "while" => {
      // Get condition
      tokens.skip("(")?;
      let condition = get_expression(tokens, 0)?;
      tokens.ignore_whitespace();
      tokens.skip(")")?;

      tokens.ignore_whitespace();
      
      // Get body
      let body = get_single_statement(tokens)?;

      // Add to parent
      if control_flow_type == "if" {
        // Check for `else` block...
        tokens.ignore_whitespace();

        let alternate = if tokens.peek_str() == "else" {
          tokens.skip_unchecked();
          Some(Box::new(get_single_statement(tokens)?))
        } else {
          None
        };
        ASTNode::StatementIf {
          condition: Box::new(condition),
          body: Box::new(body),
          alternate
        }
      } else {
        ASTNode::StatementWhile {
          condition: Box::new(condition),
          body: Box::new(body)
        }
      }
    },
    "for" => handle_for_loop(tokens)?,
    "switch" => {
      // Get condition
      tokens.skip("(")?;
      let condition = get_expression(tokens, 0)?;
      tokens.ignore_whitespace();
      tokens.skip(")")?;

      tokens.ignore_whitespace();
      tokens.skip("{")?;

      let mut cases = SmallVec::new();
      let mut default = None;

      while tokens.peek_str() != "}" {
        tokens.ignore_whitespace();
        if tokens.peek_str() == "case" {
          tokens.skip_unchecked();
          let case_condition = get_expression(tokens, 0)?;
          tokens.ignore_whitespace();
          tokens.skip(":")?;
          tokens.ignore_whitespace();
          let mut case_body = SmallVec::new();
          while !tokens.is_done() && ![ "case", "default", "}" ].contains(&tokens.peek_str()) {
            case_body.push(get_single_statement(tokens)?);
            tokens.ignore_whitespace();
          }
          cases.push((case_condition, case_body));
        } else if tokens.peek_str() == "default" {
          if default.is_some() {
            return Err(CompilerError::new(
              "Switch statements can only have one default case".to_owned(),
              tokens.consume(), tokens
            ))
          }
          tokens.skip_unchecked();
          tokens.ignore_whitespace();
          tokens.skip(":")?;
          let mut default_body = SmallVec::new();
          while !tokens.is_done() && ![ "case", "}" ].contains(&tokens.peek_str()) {
            default_body.push(get_single_statement(tokens)?);
            tokens.ignore_whitespace();
          }
          default = Some(default_body);
        } else {
          return Err(CompilerError::new(
            "Expected `case` or `default` in switch statement".to_owned(),
            tokens.consume(), tokens
          ))
        }
      }

      tokens.skip_unchecked(); // Skip "}"

      ASTNode::StatementSwitch {
        condition: Box::new(condition),
        cases,
        default
      }
    }
    other => { panic!("Control flow not implemented: {}", other); }
  }))
}

fn handle_for_loop(tokens: &mut TokenList) -> Result<ASTNode, CompilerError> {
  tokens.skip("(")?;
  tokens.ignore_whitespace();
  let init = if tokens.peek_str() == ";" {
    ASTNode::Empty
  } else if VARIABLE_DECLARATIONS.contains(&tokens.peek_str()) {
    let def_typ = get_variable_def_type(tokens)?;
    let defs = get_multiple_destructurable_declarations(tokens, false)?.0;
    ASTNode::VariableDeclaration {
      modifiers: ModifierList::new(),
      def_type: def_typ,
      defs
    }
  } else {
    get_expression(tokens, 0)?
  };
    tokens.ignore_whitespace();
    Ok(if tokens.try_skip_and_ignore_whitespace(";") {
    let condition = get_single_statement(tokens)?;
    let update = get_single_statement(tokens)?;

    tokens.skip(")")?;
    let body = get_single_statement(tokens)?;

    ASTNode::StatementFor {
      init: Box::new(init),
      condition: Box::new(condition),
      update: Box::new(update),
      body: Box::new(body)
    }
  } else if tokens.try_skip_and_ignore_whitespace("of") {
    let expression = get_expression(tokens, 0)?;

    tokens.skip(")")?;
    let body = get_single_statement(tokens)?;

    ASTNode::StatementForOf {
      init: Box::new(init),
      expression: Box::new(expression),
      body: Box::new(body)
    }
  } else if tokens.try_skip_and_ignore_whitespace("in") {
    let expression = get_expression(tokens, 0)?;
    
    tokens.skip(")")?;
    let body = get_single_statement(tokens)?;

    ASTNode::StatementForIn {
      init: Box::new(init),
      expression: Box::new(expression),
      body: Box::new(body)
    }
  } else {
    return Err(CompilerError::new(
      "Expected `;`, `of`, or `in` in for loop".to_owned(),
      tokens.peek().clone(), tokens
    ))
  })
}

// fn get_for_header(
//   tokens: &mut TokenList
// ) -> Result<Option<ASTNode>, CompilerError> {
  
// }

/// Handles import statements
fn handle_import(
  tokens: &mut TokenList
) -> Result<Option<ASTNode>, CompilerError> {
  if tokens.peek_str() != "import" {
    return Ok(None);
  }
  let checkpoint = tokens.get_checkpoint();
  tokens.skip_unchecked();
  tokens.ignore_whitespace();

  if tokens.try_skip_and_ignore_whitespace("(") {
    tokens.restore_checkpoint(checkpoint);
    return Ok(Some(get_expression(tokens, 0)?));
  } else {
    tokens.ignore_checkpoint(checkpoint);
  }

  let mut has_distinction = false;

  // Default imports `import Defaults from '...'`
  let default_alias = if tokens.peek().is_identifier() {
    let alias = Some(tokens.consume().value.to_owned());
    tokens.ignore_whitespace();
    let _ = tokens.try_skip_and_ignore_whitespace(",");
    has_distinction = true;
    alias
  } else {
    None
  };

  // Wildcard imports `import * as Identifier from '...'`
  let wildcard = if tokens.peek_str() == "*" {
    tokens.skip_unchecked();
    tokens.ignore_whitespace();
    tokens.skip("as")?;
    tokens.ignore_whitespace();
    has_distinction = true;
    Some(tokens.consume_type(TokenType::Identifier)?.value.to_owned())
  } else {
    None
  };
  tokens.ignore_whitespace();

  // Individual imports `import { A, B } from '...'`
  let mut individual = SmallVec::new();
  if tokens.peek_str() == "{" {
    tokens.skip_unchecked();
    tokens.ignore_whitespace();
    while !tokens.is_done() && tokens.peek_str() != "}" {
      let name = tokens.consume_type(TokenType::Identifier)?.value.to_owned();
      let alias = if tokens.peek_str() == "as" {
        tokens.skip_unchecked();
        tokens.ignore_whitespace();
        Some(tokens.consume_type(TokenType::Identifier)?.value.to_owned())
      } else { None };
      tokens.ignore_whitespace();
      let _ = tokens.try_skip_and_ignore_whitespace(",");
      individual.push(IndividualImport { name, alias });
    }
    tokens.skip("}")?;
    has_distinction = true;
  }
  tokens.ignore_whitespace();

  if has_distinction {
    tokens.skip("from")?;
    tokens.ignore_whitespace();
  }
  let source = tokens.consume_type(TokenType::String)?;

  Ok(Some(ASTNode::StatementImport { inner: Box::new(ImportDefinition {
    default_alias,
    wildcard,
    individual,
    source: source.value.to_owned()
  }) }))
}

/// Handles `return`, `break`, `continue`, and `throw`
fn handle_other_statements(
  tokens: &mut TokenList
) -> Result<Option<ASTNode>, CompilerError> {
  const STATEMENT_NAMES: &[&str] = &[
    "return",
    "break",
    "continue",
    "throw",
  ];
  if !STATEMENT_NAMES.contains(&tokens.peek_str()) {
    return Ok(None);
  }

  let statement_type = tokens.consume().value.to_string();
  tokens.ignore_whitespace();

  let value = if tokens.peek_str() != ";" {
    Some(Box::new(get_expression(tokens, 0)?))
  } else {
    None
  };

  Ok(Some(match statement_type.as_str() {
    "return" => ASTNode::StatementReturn { value },
    "break" => ASTNode::StatementBreak { value },
    "continue" => ASTNode::StatementContinue { value },
    "throw" => ASTNode::StatementThrow { value },
    other => { panic!("Statement not implemented: {}", other); }
  }))
}

fn handle_function_declaration<'a, 'b>(
  tokens: &'b mut TokenList,
) -> Result<Option<ASTNode>, CompilerError> where 'a: 'b {
  if tokens.peek_str() != "function" {
    return Ok(None);
  }

  tokens.skip_unchecked(); // Skip `function`

  // Get name
  tokens.ignore_whitespace();
  let name = if tokens.peek().is_identifier() {
    // Named function
    Some(tokens.consume().value.to_string())
  } else {
    // Unnamed function
    None
  };

  Ok(Some(ASTNode::FunctionDefinition {
    inner: Box::new(get_function_after_name(
      tokens,
      name
    )?)
  }))
}

/// Gets a function once the name has been consumed.
/// This includes any generics, arguments, return type, and body.
fn get_function_after_name<'a, 'b>(
  tokens: &'b mut TokenList,
  name: Option<String>
) -> Result<FunctionDefinition, CompilerError> where 'a: 'b {
  tokens.ignore_whitespace();

  let generics = get_optional_generics(tokens)?;
  
  // Get parameters and return type
  tokens.ignore_whitespace();
  tokens.skip("(")?;
  let params = get_multiple_destructurable_declarations(tokens, true)?;
  tokens.skip(")")?;
  let return_type = try_get_type(tokens)?;

  // Get body
  tokens.ignore_whitespace();
  let body = if tokens.peek_str() == ";" {
    // No body! This means it's just a declaration.
    None
  } else {
    tokens.skip("{")?;
    Some(get_block(tokens)?)
  };

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
  tokens: &'b mut TokenList,
  members: &mut SmallVec<ClassMember>
) -> Result<FunctionDefinition, CompilerError> where 'a: 'b {
  tokens.ignore_whitespace();

  // Handle generics
  if tokens.peek_str() == "<" {
    return Err(CompilerError::new(
      "Type parameters cannot appear on a constructor declaration.".to_owned(),
      tokens.consume(), tokens
    ))
  }
  
  // Get parameters
  let mut params: SmallVec<DestructurableDeclaration> = SmallVec::new();
  let mut set_properties: SmallVec<ASTNode> = SmallVec::new();
  let mut rest = Rest::new();
  tokens.skip("(")?;
  while tokens.peek_str() != ")" {
    tokens.ignore_whitespace();
    rest.try_set(tokens, true)?;
    if ACCESSIBILITY_MODIFIERS.contains(&tokens.peek_str()) {
      let modifiers = fetch_modifier_list(tokens);
      let mut declaration = get_declaration(tokens)?;
      params.push(declaration.clone().into());
      declaration.clear_value();
      members.push(ClassMember::Property(
        DeclarationComputable::from(&declaration), modifiers
      ));
      set_properties.push(ASTNode::InfixOpr {
        left: Box::new(ASTNode::InfixOpr {
          left: Box::new(ASTNode::ExprIdentifier { name: "this".to_owned() }),
          opr: ".".to_owned(),
          right: Box::new(ASTNode::ExprIdentifier { name: declaration.name().clone() })
        }),
        opr: "=".to_owned(),
        right: Box::new(ASTNode::ExprIdentifier { name: declaration.name().clone() })
      })
    } else {
      params.push(get_destructurable_declaration(tokens)?);
    }
    tokens.ignore_whitespace();
    if !tokens.try_skip_and_ignore_whitespace(",") {
      break;
    }
  }
  tokens.skip(")")?;

  // Get return type
  tokens.ignore_whitespace();
  let return_type = try_get_type(tokens)?;

  // Get body
  tokens.ignore_whitespace();
  let body = if tokens.peek_str() == ";" {
    // No body! This means it's just a declaration.
    None
  } else {
    tokens.skip("{")?;
    let mut body = get_block(tokens)?;
    match &mut body {
      ASTNode::Block { nodes } => {
        nodes.append_front(&mut set_properties);
        Some(body)
      }
      other => {
        return Err(CompilerError::new(
          format!("Expected block in function body, found {:?}", other),
          Token::from(""), tokens
        ))
      }
    }
  };

  Ok(FunctionDefinition {
    modifiers: ModifierList::new(),
    name: Some("constructor".to_owned()),
    generics: SmallVec::new(),
    params,
    rest,
    return_type,
    body
  })
}

fn handle_class_declaration<'a, 'b>(
  tokens: &'b mut TokenList
) -> Result<Option<ASTNode>, CompilerError> where 'a: 'b {
  if tokens.peek_str() != "class" {
    return Ok(None);
  }
  Ok(Some(get_class_expression(tokens)?))
}

fn get_class_expression<'a, 'b>(
  tokens: &'b mut TokenList,
) -> Result<ASTNode, CompilerError> where 'a: 'b {
  tokens.skip_unchecked(); // Skip "class"
  tokens.ignore_whitespace();

  let TypedHeader {
    name,
    generics,
    extends,
    implements
  } = get_typed_header(tokens, false)?;
  let extends = match extends.len() {
    len if len > 1 => return Err(CompilerError::new(
      "Classes can only extend once!".to_owned(),
      Token::from(""), tokens
    )),
    1 => Some(extends.last().unwrap().clone()),
    _ => None
  };

  // Classes change the way things are parsed!
  let mut kv_maps = SmallVec::new();
  let mut members: SmallVec<ClassMember> = SmallVec::new();

  tokens.ignore_whitespace();
  tokens.skip("{")?; // Skip body "{"
  tokens.ignore_whitespace();

  while tokens.peek_str() != "}" {
    // Note: here, "property" refers to anything defined with "=", which is
    // not inherently a function. "method" refers to things defined with
    // parenthesis, making them *always* be a function (or its declaration)

    // Get modifiers (if any)
    let modifiers = fetch_modifier_list(tokens);

    // Get the name (might be a property or a method, we don't know yet)

    if tokens.peek_str() == "[" {
      // Key-value map!;
      // TODO: make this accept more than single-token computed properties
      // TODO: fix computed properties
      let init_token = tokens.peek().clone();
      match parse_object_square_bracket(tokens)? {
        ObjectSquareBracketReturn::KVMap(kv_map) => {
          kv_maps.push(kv_map);
        }
        ObjectSquareBracketReturn::ComputedProp(name) => {
          let (typ, value) = get_declaration_after_name(tokens)?;
          members.push(ClassMember::Property(
            DeclarationComputable {
              name: ComputableDeclarationName::new_computed(name),
              typ: typ,
              value
            },
            modifiers.clone()
          ));
        }
        ObjectSquareBracketReturn::MappedType(..) => {
          return Err(CompilerError::new(
            "Mapped types are not allowed in class bodies".to_owned(),
            init_token, tokens
          ))
        }
      }
      
      tokens.ignore_whitespace();

      // Skip ";" (if any)
      let _ = tokens.try_skip_and_ignore_whitespace(";");
      continue;
    }

    // Check for getter/setter
    let checkpoint = tokens.get_checkpoint();
    let mut is_getter = tokens.peek_str() == "get";
    let mut is_setter = tokens.peek_str() == "set";
    if is_getter || is_setter {
      tokens.skip_unchecked();
      tokens.ignore_whitespace();
      if IGNORE_MODIFIER_IF_SYMBOL.contains(&tokens.peek_str()) {
        tokens.restore_checkpoint(checkpoint);
        is_getter = false;
        is_setter = false;
      } else {
        tokens.ignore_checkpoint(checkpoint);
      }
    } else {
      tokens.ignore_checkpoint(checkpoint);
    }

    if tokens.peek_str() == "{" && modifiers.has(Modifier::Static) {
      // Static initialization block
      tokens.skip_unchecked(); // Skip "{"
      let body = get_block(tokens)?;
      members.push(ClassMember::StaticBlock(body));
      tokens.ignore_whitespace();
      let _ = tokens.try_skip_and_ignore_whitespace(";");
      continue;
    }

    let name = tokens.consume_type(TokenType::Identifier)?;
    tokens.ignore_whitespace();

    if name.value != "constructor" && [ ":", "?", "!", "=", ";" ].contains(&tokens.peek_str()) {
      // Normal property

      if is_getter || is_setter {
        return Err(CompilerError::new(
          "Getters and setters must be followed by a method body!".to_owned(),
          tokens.peek().clone(), tokens
        ));
      }

      // Get the declaration
      let (typ, value) = get_declaration_after_name(tokens)?;
      members.push(ClassMember::Property(
        DeclarationComputable::named(name.value.to_owned(), typ, value),
        modifiers
      ));
    } else {
      // Otherwise, it's a method
      let mut function = if name.value == "constructor" {
        if is_getter || is_setter {
          return Err(CompilerError::new(
            "Constructors cannot be getters or setters!".to_owned(),
            name.clone(), tokens
          ));
        }
        get_constructor_after_name(
          tokens,
          &mut members
        )?
      } else {
        get_function_after_name(
          tokens,
          Some(name.value.to_string())
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

    tokens.ignore_whitespace();

    // Skip ";" (if any)
    if tokens.peek_str() == ";" {
      tokens.skip_unchecked();
    }

    tokens.ignore_whitespace();
  }
  tokens.skip("}")?; // Skip body "}"

  Ok(ASTNode::ClassDefinition { inner: Box::new(ClassDefinition {
    modifiers: ModifierList::new(),
    name,
    generics,
    extends,
    implements,
    kv_maps,
    members
  }) })
}

struct TypedHeader {
  name: Option<String>,
  generics: SmallVec<Type>,
  extends: SmallVec<Type>,
  implements: SmallVec<Type>,
}

/// Gets a typed header for a class or interface.
fn get_typed_header(
  tokens: &mut TokenList,
  require_name: bool
) -> Result<TypedHeader, CompilerError> {
  tokens.ignore_whitespace();
  let name = tokens.peek();
  let is_illegal_name = DISALLOWED_VARIABLE_NAMES.contains(&name.value);
  let name: Option<String> = if name.is_identifier() && !is_illegal_name {
    Some(tokens.consume().value.to_owned())
  } else if require_name {
    let t = tokens.consume();
    return Err(CompilerError::new(
      format!("Expected header name, found {:?}", t.value),
      t, tokens
    ));
  } else {
    None
  };

  let generics = get_optional_generics(tokens)?;
  tokens.ignore_whitespace();

  let extends = if tokens.peek_str() == "extends" {
    tokens.skip_unchecked(); // Skip "extends"
    get_comma_separated_types_until(tokens, &[ "implements", "{", "=" ])?
  } else {
    SmallVec::new()
  };
  tokens.ignore_whitespace();

  let implements = if tokens.peek_str() == "implements" {
    tokens.skip_unchecked(); // Skip "implements"
    get_comma_separated_types_until(tokens, &[ "{", "=" ])?
  } else {
    SmallVec::new()
  };

  Ok(TypedHeader {
    name, generics, extends, implements
  })
}

fn handle_modifiers(
  tokens: &mut TokenList
) -> Result<Option<ASTNode>, CompilerError> {
  if !MODIFIERS.contains(&tokens.peek_str()) {
    return Ok(None);
  }

  // Get the modifiers
  let modifiers = fetch_modifier_list(tokens);

  // Get the node that goes after the modifiers
  let mut node_after_modifiers = get_single_statement(tokens)?;

  node_after_modifiers.apply_modifiers(modifiers)?;
  Ok(Some(node_after_modifiers))
}

fn fetch_modifier_list(tokens: &mut TokenList) -> ModifierList {
  let mut modifiers: ModifierList = ModifierList::new();
  while MODIFIERS.contains(&tokens.peek_str()) {
    let checkpoint = tokens.get_checkpoint();
    let token = tokens.consume();
    tokens.ignore_whitespace();
    if IGNORE_MODIFIER_IF_SYMBOL.contains(&tokens.peek_str()) {
      tokens.restore_checkpoint(checkpoint);
      break;
    } else {
      tokens.ignore_checkpoint(checkpoint);
    }
    modifiers.set(match token.value {
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
  tokens: &mut TokenList
) -> Result<Option<ASTNode>, CompilerError> {
  if tokens.peek_str() != "type" {
    return Ok(None);
  }
  tokens.skip_unchecked(); // Skip `type`

  let TypedHeader {
    name, generics,
    extends, implements
  } = get_typed_header(tokens, true)?;
  let name = unsafe { name.unwrap_unchecked() };
  if !extends.is_empty() {
    return Err(CompilerError::new(
      "Type declarations can't extend!".to_owned(),
      Token::from(""), tokens
    ))
  }
  if !implements.is_empty() {
    return Err(CompilerError::new(
      "Type declarations can't implement!".to_owned(),
      Token::from(""), tokens
    ))
  }

  // Consume `=`
  tokens.ignore_whitespace();
  tokens.skip("=")?;

  // Get the second type
  let equals_typ = get_type(tokens)?;

  Ok(Some(ASTNode::InterfaceDeclaration { inner: Box::new(InterfaceDeclaration {
    modifiers: ModifierList::new(),
    name,
    generics,
    extends,
    equals_type: equals_typ
  }) }))
}

fn handle_enum(
  tokens: &mut TokenList,
  is_const: bool
) -> Result<Option<ASTNode>, CompilerError> {
  if tokens.peek_str() != "enum" {
    return Ok(None)
  }
  tokens.skip_unchecked();
  tokens.ignore_whitespace();

  let name = match tokens.consume() {
    token if token.value.len() == 0 => {
      return Err(CompilerError::new(
        "Expected enum name".to_owned(),
        token, tokens
      ))
    },
    token => token.value.to_owned()
  };

  tokens.ignore_whitespace();
  tokens.skip("{")?;

  let mut counter: u32 = 0;
  let mut members: SmallVec<(String, ASTNode)> = SmallVec::new();

  while !tokens.is_done() {
    tokens.ignore_whitespace();
    let next = tokens.peek_str();
    if next == "}" { break }

    let token = tokens.consume();
    let name = match token.typ {
      TokenType::String => token.value.to_owned(),
      TokenType::Identifier => "\"".to_owned() + token.value + "\"",
      _ => {
        return Err(CompilerError::new(
          "Expected string or identifier".to_owned(),
          token, tokens
        ))
      }
    };
    tokens.ignore_whitespace();
    let value = if tokens.peek_str() == "=" {
      tokens.skip_unchecked();
      get_expression(tokens, 1)? // TODO: why 1?
    } else {
      let number = counter.to_string();
      counter += 1;
      ASTNode::ExprNumLiteral { number }
    };
    members.push((name, value));

    tokens.ignore_whitespace();
    tokens.ignore_commas();
  }

  tokens.skip("}")?;

  Ok(Some(ASTNode::EnumDeclaration { inner: Box::new(EnumDeclaration {
    modifiers: ModifierList::new(),
    name,
    members,
    is_const,
  }) }))
}

fn handle_interface(
  tokens: &mut TokenList
) -> Result<Option<ASTNode>, CompilerError> {
  if tokens.peek_str() != "interface" {
    return Ok(None);
  }
  tokens.skip_unchecked();
  
  let TypedHeader {
    name, generics,
    extends, implements
  } = get_typed_header(tokens, true)?;
  let name = unsafe { name.unwrap_unchecked() };
  if !implements.is_empty() {
    return Err(CompilerError::new(
      "Interfaces can't implement, only extend!".to_owned(),
      Token::from(""), tokens
    ))
  }

  tokens.skip("{")?;

  // Store the parts of the interface!
  // Stores the function types in a multi-function interface
  let mut function_types = Type::Union(SmallVec::new());
  // Named key-value types
  let mut named_parts = SmallVec::new();
  // [key: type]
  let mut key_value = SmallVec::new();

  while !tokens.is_done() {
    tokens.ignore_whitespace();
    let next = tokens.peek_str();
    if next == "}" { break }

    if next == "(" {
      // Function
      let function = get_type(tokens)?;
      function_types.union(function);
    } else if next == "[" {
      // This could be either a Key-value map,
      // or a computed property
      match parse_object_square_bracket(tokens)? {
        ObjectSquareBracketReturn::KVMap(kv_map) => key_value.push(kv_map),
        ObjectSquareBracketReturn::ComputedProp(value) => {
          named_parts.push(DeclarationTyped::computed(
            value,
            try_get_type(tokens)?.unwrap_or(Type::Unknown)
          ))
        },
        ObjectSquareBracketReturn::MappedType(..) => {
          return Err(CompilerError::new(
            "Mapped types are not allowed in interface bodies".to_owned(),
            Token::from(""), tokens
          ))
        }
      }
    } else {
      // Try getting a named function
      let checkpoint = tokens.get_checkpoint();
      let function_name = tokens.consume();
      tokens.ignore_whitespace();
      // TODO: make this get a typed declaration directly (without using a type in-between)
      if tokens.peek_str() == "(" {
        // It's a function! (which is a member)
        tokens.ignore_checkpoint(checkpoint);
        let function = get_type(tokens)?;
        named_parts.push(DeclarationTyped::named(
          function_name.value.to_owned(),
          function
        ));
      } else {
        // It isn't a function, treat it as a named declaration
        tokens.restore_checkpoint(checkpoint);
        let decl = get_declaration(tokens)?;
        named_parts.push(DeclarationTyped::named(
          decl.name().clone(),
          decl.typ().clone()
        ));
      }
    }

    tokens.ignore_whitespace();
    while [ ";", "," ].contains(&tokens.peek_str()) {
      tokens.skip_unchecked();
      tokens.ignore_whitespace();
    }
  }

  tokens.skip("}")?;

  let named_dict = Type::Object {
    key_value,
    parts: named_parts
  };

  let mut equals_type = Type::Intersection(SmallVec::new());
  if function_types.inner_count() != 0 { equals_type.intersection(function_types); }
  if named_dict.inner_count() != 0 { equals_type.intersection(named_dict); }
  Ok(Some(ASTNode::InterfaceDeclaration { inner: Box::new(InterfaceDeclaration {
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
  }) }))
}

/// Handles expressions. Basically a soft wrapper around `get_expression`
fn handle_expression(
  tokens: &mut TokenList
) -> Result<ASTNode, CompilerError> {
  get_expression(tokens, 0)
}

fn parse_number(tokens: &mut TokenList) -> ASTNode {
  ASTNode::ExprNumLiteral {
    number: tokens.consume().value.to_string()
  }
}

fn parse_string(tokens: &mut TokenList) -> ASTNode {
  ASTNode::ExprStrLiteral {
    string: tokens.consume().value.to_string()
  }
}

fn parse_string_template(
  tokens: &mut TokenList
) -> Result<ASTNode, CompilerError> {
  let head_token = tokens.consume();
  let head = head_token.value[1..head_token.value.len() - 2].to_owned();

  let mut parts = SmallVec::new();

  while tokens.peek().typ != TokenType::EndOfFile {
    let expr = get_expression(tokens, 0)?;
    let literal_part = tokens.consume();
    if literal_part.typ != TokenType::StringTemplateMiddle && literal_part.typ != TokenType::StringTemplateEnd {
      return Err(CompilerError::new(
        "Expected string template part".to_owned(),
        literal_part, tokens
      ));
    }
    parts.push((expr, match literal_part.typ {
      TokenType::StringTemplateMiddle => literal_part.value[1..literal_part.value.len() - 2].to_owned(),
      TokenType::StringTemplateEnd => literal_part.value[1..literal_part.value.len() - 1].to_owned(),
      _ => unreachable!()
    }));
    if literal_part.typ == TokenType::StringTemplateEnd { break; }
    if tokens.peek().typ == TokenType::EndOfFile {
      return Err(CompilerError::new(
        "Unterminated template literal".to_owned(),
        tokens.peek().clone(), tokens
      ));
    }
  }

  Ok(ASTNode::ExprTemplateLiteral { head, parts })
}

fn parse_name(
  tokens: &mut TokenList
) -> Result<ASTNode, CompilerError> {
  if tokens.peek_str() == "function" {
    // Handle inline functions
    return Ok(handle_function_declaration(tokens)?.unwrap());
  }

  match tokens.consume().value {
    "true" => { Ok(ASTNode::ExprBoolLiteral { value: true }) }
    "false" => { Ok(ASTNode::ExprBoolLiteral { value: false }) }
    name => { Ok(ASTNode::ExprIdentifier { name: name.to_string() }) }
  }
}

fn parse_prefix(
  tokens: &mut TokenList,
  precedence: u8
) -> Result<ASTNode, CompilerError> {
  let prefix_start = tokens.consume().value.to_string();

  let is_grouping = [ "(", "[" ].contains(&prefix_start.as_str());
  if is_grouping {
    // Groupings get special treatment!
    let nodes = separate_commas(get_expression(tokens, 0)?);
    let ret = match prefix_start.as_str() {
      "(" => ASTNode::Parenthesis { nodes },
      "[" => ASTNode::Array { nodes },
      unknown_grouping => {
        panic!("Grouping not implemented: {}", unknown_grouping);
      }
    };
    let group_end = INVERSE_GROUPINGS[prefix_start.as_str()];
    tokens.skip(group_end)?; // Skip grouping close ")" or "]"
    Ok(ret)
  } else if prefix_start == "{" {
    // Dicts get even more special treatment!
    let mut properties = SmallVec::new();
    loop {
      tokens.ignore_whitespace();
      let key_token = tokens.peek();
      if key_token.value == "}" { break; }

      if key_token.value == "..." {
        // Spread
        tokens.skip_unchecked(); // Skip "..."

        properties.push(ObjectProperty::Rest {
          argument: get_expression(tokens, 1)?
        });
      } else {
        // String, identifier, or computed key
        let (computed, key) = match (&key_token.typ, key_token.value) {
          (TokenType::String, _) => {
            (false, ASTNode::ExprStrLiteral { string: tokens.consume().value.to_string() })
          },
          (TokenType::Identifier, _) => {
            (false, ASTNode::ExprIdentifier { name: tokens.consume().value.to_string() })
          }
          (TokenType::Symbol, "[") => {
            // Computed key
            tokens.skip_unchecked();
            let inner_key = get_expression(tokens, 0)?;
  
            tokens.ignore_whitespace();
            tokens.skip("]")?;
  
            (true, inner_key)
          },
          other => {
            return Err(CompilerError::new(
              format!("Expected property assignment, found {:?}", other),
              key_token.clone(), tokens
            ));
          }
        };
  
        // Skip ":"
        tokens.ignore_whitespace();
        if let ASTNode::ExprIdentifier { name } = &key {
          if tokens.peek_str() != ":" {
            properties.push(ObjectProperty::Shorthand { key: name.clone() });
            tokens.ignore_commas();
            continue;
          }
          tokens.skip(":")?;
        } else {
          tokens.skip(":")?;
        }
  
        // Get value
        let value = get_expression(tokens, 1)?;
        
        properties.push(ObjectProperty::Property {
          computed,
          key,
          value
        });
      }

      // Ignore trailing commas (if any)
      tokens.ignore_commas();
    }
    tokens.skip("}")?;

    Ok(ASTNode::Dict { properties })
  } else {
    Ok(ASTNode::PrefixOpr {
      opr: prefix_start,
      expr: Box::new(get_expression(tokens, precedence)?)
    })
  }
}

/// Parses an infix operator. Ran when the current token is known to be infix.
fn parse_infix<'a, 'b>(
  left: ASTNode,
  tokens: &'b mut TokenList,
  precedence: u8
) -> Result<ASTNode, CompilerError> where 'a: 'b {

  if let ASTNode::ExprIdentifier { name } = &left {
    if name == "async" {
      if tokens.peek_str() == "(" {
        let mut arrow_fn = parse_arrow_function(tokens)?;
        match &mut arrow_fn {
          ASTNode::ArrowFunctionDefinition { inner } => {
            inner.is_async = true;
          }
          _ => unreachable!()
        }
        return Ok(arrow_fn);
      }
    }
  }

  let opr_token = tokens.consume();
  let opr = opr_token.value.to_string();

  if opr == "(" || opr == "[" {
    // Function call or property access (indexing)

    // Get children
    let inner = get_expression(tokens, 0)?;
    let group_end = INVERSE_GROUPINGS[opr.as_str()];
    tokens.skip(group_end)?; // Skip trailing group

    match opr.as_str() {
      "(" => {
        if let ASTNode::ExprIdentifier { name } = &left {
          if name == "import" {
            // Dynamic import
            return Ok(ASTNode::ExpressionImport { value: Box::new(inner) });
          }
        }
        return Ok(ASTNode::ExprFunctionCall {
          callee: Box::new(left),
          generics: SmallVec::new(),
          arguments: separate_commas(inner)
        })
      },
      "[" => return Ok(ASTNode::ExprIndexing {
        callee: Box::new(left),
        property: Box::new(inner)
      }),
      other => { panic!("Grouping not implemented: {}", other); }
    };
  } else if opr == "=>" {
    // Arrow functions get special treatment!
    // This captures arrow functions that don't start with parenthesis, or those
    // which don't have an arrow-function-specific header.

    let params = match left {
      ASTNode::ExprIdentifier { name } => {
        SmallVec::with_element(Declaration::new(name, Type::Unknown, None))
      }
      ASTNode::Parenthesis { nodes } => {
        let mut params = SmallVec::new();
        for n in nodes {
          params.push(match n {
            ASTNode::ExprIdentifier { name } => Declaration::new(name, Type::Unknown, None),
            ASTNode::InfixOpr {
              left, opr, right
            } if opr == "=" && matches!(*left, ASTNode::ExprIdentifier { .. }) => match *left {
              ASTNode::ExprIdentifier { name } => Declaration::new(name, Type::Unknown, Some(*right)),
              _ => unreachable!()
            }
            other => return Err(CompilerError::new(
              format!("Arrow function expected parameter, found {:?}", other),
              other.as_token(), tokens
            ))
          });
        }
        params
      }
      other => {
        return Err(CompilerError::new(
          format!(
            "Arrow function expected parenthesis or identifier, found {:?}",
            other
          ),
          other.as_token(), tokens
        ))
      }
    };

    return Ok(parse_arrow_function_after_arrow(tokens, params, Rest::new(), Type::Unknown)?);
  } else if opr == "?" {
    // Ternary (eg. `condition ? true : false`)

    // Get the first part, up until the `:`
    let mut if_true = get_expression(tokens, *crate::operations::COLON_PRECEDENCE)?;
    while tokens.peek_str() != ":" {
      if tokens.is_done() {
        return Err(CompilerError::new(
          "Ternary not closed!".to_string(),
          tokens.consume(), tokens
        ));
      }
      if_true = parse_infix(
        if_true,
        tokens,
        *crate::operations::COLON_PRECEDENCE
      )?;
      tokens.ignore_whitespace();
    }
    tokens.skip_unchecked();
    let if_false = get_expression(tokens, precedence)?;

    return Ok(ASTNode::ExprTernary {
      condition: Box::new(left),
      if_true: Box::new(if_true),
      if_false: Box::new(if_false)
    });
  } else if opr == "<" {
    // An inline "<" could potentially be a generic argument!
    let checkpoint = tokens.get_checkpoint();

    // Try parsing generics...
    if let Ok(mut generics) = get_generics(tokens) {
      if tokens.peek_str() == "(" {
        tokens.ignore_checkpoint(checkpoint);
        let mut fn_call = parse_infix(left, tokens, precedence)?;
        if let ASTNode::ExprFunctionCall { generics: inner_generics, .. } = &mut fn_call {
          inner_generics.append(&mut generics);
        }
        return Ok(fn_call)
      }
    }

    // Since this didn't work, go back
    tokens.restore_checkpoint(checkpoint);
  }

  // Normal infix
  Ok(ASTNode::InfixOpr {
    left: Box::new(left),
    opr,
    right: Box::new(get_expression(tokens, precedence)?)
  })
}

/// Turns a tree containing commas into a vector with the nodes they separated
fn separate_commas(node: ASTNode) -> SmallVec<ASTNode> {
  match node {
    // Separate infix operations (but only if the operation is ",")
    ASTNode::InfixOpr { left, ref opr, right } if opr == "," => {
      // The size of the output vec is calculated from the sum
      // of both sides, so there's no wasted capacity.
      let mut nodes = separate_commas(*left);
      nodes.append(&mut separate_commas(*right));
      nodes
    }

    // Otherwise, return the node, as it's between commas
    _ => SmallVec::with_element(node)
  }
}

/// Tries parsing an arrow function, returning CompilerError if none is present.
/// Caller is responsible for restoring the tokenizer back to its origial state
/// if this function doesn't return successfully.
fn parse_arrow_function(
  tokens: &mut TokenList,
) -> Result<ASTNode, CompilerError> {
  tokens.skip("(")?;
  let (params, spread) = get_multiple_declarations(tokens, true)?;
  tokens.skip(")")?;
  tokens.ignore_whitespace();
  let return_type = try_get_type(tokens)?;
  tokens.skip("=>")?;
  Ok(parse_arrow_function_after_arrow(tokens, params, spread, return_type.unwrap_or(Type::Unknown))?)
}

/// Parses an arrow function starting at the arrow (`=>`)
fn parse_arrow_function_after_arrow(
  tokens: &mut TokenList,
  params: SmallVec<Declaration>,
  rest: Rest,
  return_type: Type
) -> Result<ASTNode, CompilerError> {
  tokens.ignore_whitespace();
  let is_expression = tokens.peek_str() != "{";
  let body = if is_expression {
    get_expression(tokens, *crate::operations::ARROW_FN_PRECEDENCE)?
  } else {
    tokens.skip_unchecked(); // Skip "{"
    get_block(tokens)?
  };

  Ok(ASTNode::ArrowFunctionDefinition { inner: Box::new(ArrowFunctionDefinition {
    is_async: false,
    params,
    rest,
    return_type,
    body
  }) })
}

/// Gets a single expression
pub fn get_expression<'a, 'b>(
  tokens: &'b mut TokenList,
  precedence: u8
) -> Result<ASTNode, CompilerError> where 'a: 'b {
  tokens.ignore_whitespace();
  
  // Get left (or sometimes only) side (which can be the prexfix!)
  let mut left = {
    let next = tokens.peek();

    if [ ")", "]", "}", ",", ";" ].contains(&next.value) || next.typ == TokenType::EndOfFile {
      // End it here!
      return Ok(ASTNode::Empty);
    }

    match next.typ {
      TokenType::Number => parse_number(tokens),
      TokenType::String => parse_string(tokens),
      TokenType::StringTemplateStart => parse_string_template(tokens)?,
      TokenType::Symbol | TokenType::Identifier => {
        // `class` can be used inside an expression, but calling it a
        // prefix feels strange... I'm going to handle it here
        if next.value == "class" {
          get_class_expression(tokens)?
        } else if next.value == "function" {
          tokens.skip_unchecked(); // Skip "function"
          let function = get_function_after_name(tokens, None)?;
          ASTNode::FunctionDefinition { inner: Box::new(function) }
        } else {
          let binding_power = get_operator_binding_power(
            ExprType::Prefx,
            next.value
          );
          if let Some(binding_power) = binding_power {
            // These prefix operators include what you might expect (+, -, ~),
            // along with arrays, dicts, and parenthesis!
            if next.value == "(" {
              // A parenthesis could be either a normal expression, or an arrow
              // function. Since normal expressions are more common,
              // we try parsing those first.

              // Check if it ends with a type!
              let checkpoint = tokens.get_checkpoint();
              let mut paren_nesting = 1;
              tokens.skip_unchecked(); // Skip `(`
              while paren_nesting != 0 {
                match tokens.consume().value {
                  "(" => paren_nesting += 1,
                  ")" => paren_nesting -= 1,
                  _ => {}
                }
              }
              tokens.ignore_whitespace();
              let is_arrow_function = ["=>", ":"].contains(&tokens.peek_str()) && precedence <= *ARROW_FN_PRECEDENCE;
              tokens.restore_checkpoint(checkpoint);

              if is_arrow_function {
                println!("Precedence: {}", precedence);
                parse_arrow_function(tokens)?
              } else {
                parse_prefix(tokens, binding_power.1)?
              }
            } else {
              parse_prefix(tokens, binding_power.1)?
            }
          } else if next.is_identifier() {
            // Could be a name...
            parse_name(tokens)?
          } else {
            // Not a name & no matching operators.
            return Err(CompilerError::new(
              "Prefix operator not found".to_owned(),
              tokens.consume(), tokens
            ));
          }
        }
      },
      _ => {
        return Err(CompilerError::new(
          format!(
            "Unexpected token when parsing expression: {:?}",
            next
          ),
          tokens.consume(), tokens
        ));
      },
    }
  };

  // If there's a next operator, parse it
  loop {
    // Get the next token
    tokens.ignore_whitespace();
    let next = tokens.peek();
    if next.typ == TokenType::EndOfFile { break; }

    // println!("Handling infix/postfix (w/ min precedence {}): {:?}", precedence, next);

    // `as` statements get special treatment
    if next.value == "as" {
      tokens.skip_unchecked();
      let cast_type = get_type(tokens)?;
      left = ASTNode::ExprAs {
        value: Box::new(left),
        cast_type: Box::new(cast_type)
      };
      continue;
    }

    // Handle postfix

    if let Some(binding_power) = get_operator_binding_power(
      ExprType::Pstfx,
      next.value
    ) {
      if binding_power.0 < precedence {
        break;
      }
      left = if tokens.peek_str() == "!" {
        // Non-null assertion
        tokens.skip_unchecked(); // Skip "!"
        ASTNode::NonNullAssertion { expr: Box::new(left) }
      } else {
        // Any other postfix operator
        ASTNode::PostfixOpr {
          expr: Box::new(left),
          opr: tokens.consume().value.to_string()
        }
      };
      continue;
    }

    // Handle infix

    let binding_power = if let Some(binding_power) = get_operator_binding_power(
      ExprType::Infx,
      next.value
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
    left = parse_infix(left, tokens, binding_power.1)?;
  }
  Ok(left)
}

