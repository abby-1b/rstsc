use phf::{phf_map, phf_set};
use crate::{
  ast::{
    ASTNode, ArrowFunctionDefinition, ClassDefinition, EnumDeclaration, FunctionDefinition, InterfaceDeclaration, ObjectProperty
  }, ast_common::{Modifier, ModifierList, VariableDefType, ACCESSIBILITY_MODIFIERS, MODIFIERS}, declaration::{Declaration, DeclarationTyped}, error_type::CompilerError, operations::{get_operator_binding_power, ExprType}, small_vec::SmallVec, spread::Spread, tokenizer::{Token, TokenList, TokenType}, types::{
    get_comma_separated_types_until, get_generics,
    get_key_value_or_computed_property, get_optional_generics, get_type,
    try_get_type, KVMapOrComputedProp, Type
  }
};

lazy_static::lazy_static! {
  static ref ARROW_FN_PRECEDENCE: u8 = get_operator_binding_power(ExprType::Infx, "=>").unwrap().1;
  static ref COLON_PRECEDENCE: u8 = get_operator_binding_power(ExprType::Special, ":").unwrap().1;
}

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
pub fn get_block<'a>(tokens: &mut TokenList<'a>) -> Result<ASTNode, CompilerError<'a>> {
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
  tokens: &'b mut TokenList<'a>
) -> Result<ASTNode, CompilerError<'a>> where 'a: 'b {
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
fn handle_blocks<'a>(
  tokens: &mut TokenList<'a>
) -> Result<Option<ASTNode>, CompilerError<'a>> {
  if tokens.peek_str() != "{" {
    return Ok(None);
  }
  tokens.skip_unchecked(); // Skip "{"
  Ok(Some(get_block(tokens)?))
}

/// Handles variable initialization
fn handle_vars<'a, 'b>(
  tokens: &'b mut TokenList<'a>
) -> Result<Option<ASTNode>, CompilerError<'a>> where 'a: 'b {
  const VARIABLE_DECLARATIONS: &[&str] = &[ "var", "let", "const" ];
  if !VARIABLE_DECLARATIONS.contains(&tokens.peek_str()) {
    return Ok(None);
  }

  let def_type = get_variable_def_type(tokens)?;
  tokens.ignore_whitespace();

  if matches!(def_type, VariableDefType::Const) && tokens.peek_str() == "enum" {
    // Const enums
    return handle_enum(tokens, true);
  }

  // Get the variable definitions
  let defs = get_multiple_declarations(tokens, false)?.0;

  Ok(Some(ASTNode::VariableDeclaration {
    modifiers: Default::default(),
    def_type,
    defs
  }))
}

fn get_variable_def_type<'b>(
  tokens: &mut TokenList<'b>
) -> Result<VariableDefType, CompilerError<'b>> {
  // Get the header
  let header_token = tokens.consume();
  match header_token.value {
    "var"   => Ok(VariableDefType::Var),
    "let"   => Ok(VariableDefType::Let),
    "const" => Ok(VariableDefType::Const),
    other => Err(CompilerError {
      message: format!("Unexpected var declaration: {}", other),
      token: header_token
    })
  }
}

/// Gets a single named (and optionally typed) declaration,
/// with or without a value. Does NOT handle spreads!
fn get_declaration<'a, 'b>(
  tokens: &'b mut TokenList<'a>
) -> Result<Declaration, CompilerError<'a>> where 'a: 'b {
  tokens.ignore_whitespace();
  let name_token = tokens.consume();
  if !name_token.is_identifier() {
    return Err(CompilerError {
      message: "Expected identifier".to_owned(),
      token: name_token
    })
  }
  let name = name_token.value.to_owned();
  let typ = if tokens.peek_str() == "?" {
    let conditional_token = tokens.consume();
    tokens.ignore_whitespace();
    if tokens.peek_str() != ":" {
      return Err(CompilerError {
        message: "Expected `:` after `?` in conditional declaration".to_owned(),
        token: conditional_token
      })
    }
    let mut typ = get_type(tokens)?;
    typ.intersection(Type::Void);
    Some(typ)
  } else {
    try_get_type(tokens)?
  }.unwrap_or(Type::Unknown);

  tokens.ignore_whitespace();
  let value = if tokens.peek_str() == "=" {
    tokens.skip_unchecked();
    Some(get_expression(tokens, 0)?)
  } else {
    None
  };
  Ok(Declaration::new(name, typ, value))
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
  tokens: &'b mut TokenList<'a>,
  allow_spread: bool
) -> Result<(SmallVec<Declaration>, Spread), CompilerError<'a>> where 'a: 'b {
  tokens.ignore_whitespace();
  let mut declarations = SmallVec::new();
  let mut spread = Spread::new();
  while ![ ";", ")" ].contains(&tokens.peek_str()) {
    if tokens.peek_str() == "..." {
      if !allow_spread { return Err(CompilerError {
        message: "Unexpected spread".to_owned(),
        token: tokens.consume()
      }) }
      spread.set(declarations.len_natural(), tokens.consume())?;
    }
    declarations.push(get_declaration(tokens)?);

    if !tokens.ignore_commas() { break }
  }
  Ok((declarations, spread))
}

/// Handles control flow, like `if`, `while`, and `for`
fn handle_control_flow<'a>(
  tokens: &mut TokenList<'a>
) -> Result<Option<ASTNode>, CompilerError<'a>> {
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
    "for" => {
      // TODO: handle `for async`

      // Get header
      tokens.skip("(")?;
      let init = get_single_statement(tokens)?;
      let condition = get_single_statement(tokens)?;
      let update = get_single_statement(tokens)?;
      tokens.skip(")")?;

      // Get body
      let body = get_single_statement(tokens)?;

      ASTNode::StatementFor {
        init: Box::new(init),
        condition: Box::new(condition),
        update: Box::new(update),
        body: Box::new(body)
      }
    },
    // "switch" => {},
    other => { panic!("Control flow not implemented: {}", other); }
  }))
}

/// Handles `return`, `break`, `continue`, and `throw`
fn handle_other_statements<'a>(
  tokens: &mut TokenList<'a>
) -> Result<Option<ASTNode>, CompilerError<'a>> {
  const STATEMENT_NAMES: &[&str] = &[
    "return",
    "break",
    "continue",
    "throw"
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
  tokens: &'b mut TokenList<'a>,
) -> Result<Option<ASTNode>, CompilerError<'a>> where 'a: 'b {
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
  tokens: &'b mut TokenList<'a>,
  name: Option<String>
) -> Result<FunctionDefinition, CompilerError<'a>> where 'a: 'b {
  tokens.ignore_whitespace();

  let generics = get_optional_generics(tokens)?;
  
  // Get parameters and return type
  tokens.ignore_whitespace();
  tokens.skip("(")?;
  let params = get_multiple_declarations(tokens, true)?;
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
    modifiers: Default::default(),
    name,
    generics,
    params: params.0,
    spread: params.1,
    return_type,
    body
  })
}

/// Similar to `get_function_after_name`, gets a class constructor.
fn get_constructor_after_name<'a, 'b>(
  tokens: &'b mut TokenList<'a>,
  declarations: &mut SmallVec<(ModifierList, Declaration)>
) -> Result<FunctionDefinition, CompilerError<'a>> where 'a: 'b {
  tokens.ignore_whitespace();

  // Handle generics
  if tokens.peek_str() == "<" {
    return Err(CompilerError {
      message: "Type parameters cannot appear on a constructor declaration.".to_owned(),
      token: tokens.consume()
    })
  }
  
  // Get parameters
  let mut params = SmallVec::new();
  let mut set_properties: SmallVec<ASTNode> = SmallVec::new();
  let mut spread = Spread::new();
  tokens.skip("(")?;
  while tokens.peek_str() != ")" {
    tokens.ignore_whitespace();
    if ACCESSIBILITY_MODIFIERS.contains(&tokens.peek_str()) {
      let modifiers = fetch_modifier_list(tokens);
      let mut declaration = get_declaration(tokens)?;
      spread.try_set(tokens, params.len_natural())?;
      params.push(declaration.clone());
      declaration.clear_value();
      declarations.push((modifiers, declaration.clone()));
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
      params.push(get_declaration(tokens)?);
    }
  }
  tokens.skip_unchecked(); // Skip ")"

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
        return Err(CompilerError {
          message: format!(
            "Expected block in function body, found {:?}",
            other
          ),
          token: Token::from("")
        })
      }
    }
  };

  Ok(FunctionDefinition {
    modifiers: Default::default(),
    name: Some("constructor".to_owned()),
    generics: SmallVec::new(),
    params,
    spread,
    return_type,
    body
  })
}

fn handle_class_declaration<'a, 'b>(
  tokens: &'b mut TokenList<'a>
) -> Result<Option<ASTNode>, CompilerError<'a>> where 'a: 'b {
  if tokens.peek_str() != "class" {
    return Ok(None);
  }
  Ok(Some(get_class_expression(tokens)?))
}

fn get_class_expression<'a, 'b>(
  tokens: &'b mut TokenList<'a>,
) -> Result<ASTNode, CompilerError<'a>> where 'a: 'b {
  tokens.skip_unchecked(); // Skip "class"
  tokens.ignore_whitespace();

  let TypedHeader {
    name,
    generics,
    extends,
    implements
  } = get_typed_header(tokens, false)?;
  let extends = match extends.len() {
    len if len > 1 => return Err(CompilerError {
      message: "Classes can only extend once!".to_owned(),
      token: Token::from("")
    }),
    1 => Some(extends.last().unwrap().clone()),
    _ => None
  };

  // Classes change the way things are parsed!
  let mut kv_maps = SmallVec::new();
  let mut declarations = SmallVec::new();
  let mut methods = SmallVec::new();

  tokens.ignore_whitespace();
  tokens.skip("{")?; // Skip body "{"
  tokens.ignore_whitespace();

  while tokens.peek_str() != "}" {
    // Note: here, "property" refers to anything defined with "=", which is
    // not inherently a function. "method" refers to things defined with
    // parenthesis, making them *always* be a function (or its declaration)

    // Get modifiers (if any)
    let modifiers = fetch_modifier_list(tokens);

    let checkpoint = tokens.get_checkpoint();

    // Get the name (might be a property or a method, we don't know yet)
    if tokens.peek_str() == "[" {
      tokens.ignore_checkpoint(checkpoint);
      // Key-value map!;
      // TODO: make this accept more than single-token computed properties
      // TODO: fix computed properties
      match get_key_value_or_computed_property(tokens)? {
        KVMapOrComputedProp::KVMap(kv_map) => {
          kv_maps.push(kv_map);
        }
        KVMapOrComputedProp::ComputedProp(..) => {
          todo!();
        }
      }
      
      tokens.ignore_whitespace();

      // Skip ";" (if any)
      if tokens.peek_str() == ";" {
        tokens.skip_unchecked();
      }

      tokens.ignore_whitespace();
      continue;
    }
    let name = tokens.consume();
    if !name.is_identifier() {
      tokens.ignore_checkpoint(checkpoint);
      return Err(CompilerError {
        message: "Expected identifier in class body!".to_string(),
        token: name
      })
    }
    tokens.ignore_whitespace();

    if name.value != "constructor" && [ ":", "=", ";" ].contains(&tokens.peek_str()) {
      // Normal property
      tokens.restore_checkpoint(checkpoint);

      // Get the declaration
      let gotten_declarations = get_multiple_declarations(tokens, false)?.0;

      // Add the declaration
      for declaration in gotten_declarations {
        declarations.push((
          modifiers.clone(),
          declaration
        ));
      }
    } else {
      // Otherwise, it's a method
      tokens.ignore_checkpoint(checkpoint);
      let mut function = if name.value == "constructor" {
        get_constructor_after_name(
          tokens,
          &mut declarations
        )?
      } else {
        get_function_after_name(
          tokens,
          Some(name.value.to_string())
        )?
      };
      function.modifiers.flags |= modifiers.flags;
      if !function.modifiers.has(Modifier::Public) &&
        !function.modifiers.has(Modifier::Private) &&
        !function.modifiers.has(Modifier::Protected) {
        function.modifiers.set(Modifier::Public);
      }
      methods.push(function);
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
    modifiers: Default::default(),
    name,
    generics,
    extends,
    implements,
    kv_maps,
    declarations,
    methods
  }) })
}

struct TypedHeader {
  name: Option<String>,
  generics: SmallVec<Type>,
  extends: SmallVec<Type>,
  implements: SmallVec<Type>,
}

/// Gets a typed header for a class or interface.
fn get_typed_header<'a>(
  tokens: &mut TokenList<'a>,
  require_name: bool
) -> Result<TypedHeader, CompilerError<'a>> {
  tokens.ignore_whitespace();
  let name = tokens.peek();
  let is_illegal_name = DISALLOWED_VARIABLE_NAMES.contains(&name.value);
  let name: Option<String> = if name.is_identifier() && !is_illegal_name {
    Some(tokens.consume().value.to_owned())
  } else if require_name {
    let t = tokens.consume();
    return Err(CompilerError {
      message: format!("Expected header name, found {:?}", t.value),
      token: t
    });
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

fn handle_modifiers<'a>(
  tokens: &mut TokenList<'a>
) -> Result<Option<ASTNode>, CompilerError<'a>> {
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
  let mut modifiers: ModifierList = Default::default();
  while MODIFIERS.contains(&tokens.peek_str()) {
    match tokens.consume().value {
      "export" => { modifiers.set(Modifier::Export); },
      "async" => { modifiers.set(Modifier::Async); },
      "static" => { modifiers.set(Modifier::Static); },
      "public" => { modifiers.set(Modifier::Public); },
      "private" => { modifiers.set(Modifier::Private); },
      "protected" => { modifiers.set(Modifier::Protected); },
      "readonly" => { modifiers.set(Modifier::Readonly); },
      "abstract" => { modifiers.set(Modifier::Abstract); },
      other => { panic!("Modifier not implemented: {:?}", other); }
    };
    tokens.ignore_whitespace();
  }
  modifiers
}

fn handle_type_declaration<'a>(
  tokens: &mut TokenList<'a>
) -> Result<Option<ASTNode>, CompilerError<'a>> {
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
    return Err(CompilerError {
      message: "Type declarations can't extend!".to_owned(),
      token: Token::from("")
    })
  }
  if !implements.is_empty() {
    return Err(CompilerError {
      message: "Type declarations can't implement!".to_owned(),
      token: Token::from("")
    })
  }

  // Consume `=`
  tokens.ignore_whitespace();
  tokens.skip("=")?;

  // Get the second type
  let equals_typ = get_type(tokens)?;

  Ok(Some(ASTNode::InterfaceDeclaration { inner: Box::new(InterfaceDeclaration {
    name,
    generics,
    extends,
    equals_type: equals_typ
  }) }))
}

fn handle_enum<'a>(
  tokens: &mut TokenList<'a>,
  is_const: bool
) -> Result<Option<ASTNode>, CompilerError<'a>> {
  if tokens.peek_str() != "enum" {
    return Ok(None)
  }
  tokens.skip_unchecked();
  tokens.ignore_whitespace();

  let name = match tokens.consume() {
    token if token.value.len() == 0 => {
      return Err(CompilerError {
        message: "Expected enum name".to_owned(),
        token: token
      })
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
        return Err(CompilerError {
          message: "Expected string or identifier".to_owned(),
          token: token
        })
      }
    };
    tokens.ignore_whitespace();
    let value = if tokens.peek_str() == "=" {
      tokens.skip_unchecked();
      get_expression(tokens, 1)?
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
    name,
    members,
    is_const,
  }) }))
}

fn handle_interface<'a>(
  tokens: &mut TokenList<'a>
) -> Result<Option<ASTNode>, CompilerError<'a>> {
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
    return Err(CompilerError {
      message: "Interfaces can't implement, only extend!".to_owned(),
      token: Token::from("")
    })
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
      match get_key_value_or_computed_property(tokens)? {
        KVMapOrComputedProp::KVMap(kv_map) => key_value.push(kv_map),
        KVMapOrComputedProp::ComputedProp(value) => {
          named_parts.push(DeclarationTyped::computed(
            value,
            try_get_type(tokens)?.unwrap_or(Type::Unknown)
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
fn handle_expression<'a>(
  tokens: &mut TokenList<'a>
) -> Result<ASTNode, CompilerError<'a>> {
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

fn parse_name<'a>(
  tokens: &mut TokenList<'a>
) -> Result<ASTNode, CompilerError<'a>> {
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

fn parse_prefix<'a>(
  tokens: &mut TokenList<'a>,
  precedence: u8
) -> Result<ASTNode, CompilerError<'a>> {
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

        properties.push(ObjectProperty::Spread {
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
            return Err(CompilerError {
              message: format!("Expected key, found {:?}", other),
              token: key_token.clone()
            });
          }
        };
  
        // Skip ":"
        tokens.ignore_whitespace();
        tokens.skip(":")?;
  
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
  tokens: &'b mut TokenList<'a>,
  precedence: u8
) -> Result<ASTNode, CompilerError<'a>> where 'a: 'b {
  let opr_token = tokens.consume();
  let opr = opr_token.value.to_string();

  if opr == "(" || opr == "[" {
    // Function call or property access (indexing)

    // Get children
    let inner = get_expression(tokens, 0)?;
    let group_end = INVERSE_GROUPINGS[opr.as_str()];
    tokens.skip(group_end)?; // Skip trailing group

    match opr.as_str() {
      "(" => return Ok(ASTNode::ExprFunctionCall {
        callee: Box::new(left),
        generics: SmallVec::new(),
        arguments: separate_commas(inner)
      }),
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
              _ => panic!() // This won't happen
            }
            other => return Err(CompilerError {
              message: format!("Arrow function expected parameter, found {:?}", other),
              token: other.as_token()
            })
          });
        }
        params
      }
      other => {
        return Err(CompilerError {
          message: format!(
            "Arrow function expected parenthesis or identifier, found {:?}",
            other
          ),
          token: other.as_token()
        })
      }
    };

    return Ok(parse_arrow_function_after_arrow(tokens, params, Spread::new(), Type::Unknown)?);
  } else if opr == "?" {
    // Ternary (eg. `condition ? true : false`)

    // Get the first part, up until the `:`
    let mut if_true = get_expression(tokens, *COLON_PRECEDENCE)?;
    while tokens.peek_str() != ":" {
      if tokens.is_done() {
        return Err(CompilerError {
          message: "Ternary not closed!".to_string(),
          token: tokens.consume()
        });
      }
      if_true = parse_infix(
        if_true,
        tokens,
        *COLON_PRECEDENCE
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

/// Tries parsing an arrow function, returning None if none is present.
/// Caller is responsible for restoring the tokenizer back to its origial state
/// if this function returns None.
fn parse_arrow_function<'a>(
  tokens: &mut TokenList<'a>,
) -> Result<ASTNode, CompilerError<'a>> {
  tokens.skip("(")?;
  let (params, spread) = get_multiple_declarations(tokens, true)?;
  tokens.skip(")")?;
  tokens.ignore_whitespace();
  let return_type = try_get_type(tokens)?;
  tokens.skip("=>")?;
  Ok(parse_arrow_function_after_arrow(tokens, params, spread, return_type.unwrap_or(Type::Unknown))?)
}

/// Parses an arrow function starting at the arrow (`=>`)
fn parse_arrow_function_after_arrow<'a>(
  tokens: &mut TokenList<'a>,
  params: SmallVec<Declaration>,
  spread: Spread,
  return_type: Type
) -> Result<ASTNode, CompilerError<'a>> {
  tokens.ignore_whitespace();
  let is_expression = tokens.peek_str() != "{";
  let body = if is_expression {
    get_expression(tokens, *ARROW_FN_PRECEDENCE)?
  } else {
    tokens.skip_unchecked(); // Skip "{"
    get_block(tokens)?
  };

  Ok(ASTNode::ArrowFunctionDefinition { inner: Box::new(ArrowFunctionDefinition {
    params,
    spread,
    return_type,
    body
  }) })
}

/// Gets a single expression
pub fn get_expression<'a, 'b>(
  tokens: &'b mut TokenList<'a>,
  precedence: u8
) -> Result<ASTNode, CompilerError<'a>> where 'a: 'b {
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
      TokenType::Symbol | TokenType::Identifier => {
        // `class` can be used inside an expression, but calling it a
        // prefix feels strange... I'm going to handle it here
        if next.value == "class" {
          get_class_expression(tokens)?
        } else if next.value == "function" {
          tokens.skip_unchecked(); // Skip "function"
          let function = get_function_after_name(tokens, None)?;
          dbg!(&function);
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
              let mut skipped_tokens = 0;
              while paren_nesting != 0 {
                skipped_tokens += 1;
                match tokens.consume().value {
                  "(" => paren_nesting += 1,
                  ")" => paren_nesting -= 1,
                  _ => {}
                }
              }
              dbg!(skipped_tokens);
              tokens.ignore_whitespace();
              let is_arrow_function = ["=>", ":"].contains(&tokens.peek_str());
              tokens.restore_checkpoint(checkpoint);

              if is_arrow_function {
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
            return Err(CompilerError {
              message: "Prefix operator not found".to_owned(),
              token: tokens.consume()
            });
          }
        }
      },
      _ => {
        return Err(CompilerError {
          message: format!(
            "Unexpected token when parsing expression: {:?}",
            next
          ),
          token: tokens.consume()
        });
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

