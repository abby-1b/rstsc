use std::collections::hash_map::DefaultHasher;
use std::fmt::{Display, Debug};
use std::hash::{Hash, Hasher};

use crate::ast::ASTNode;
use crate::declaration::{ComputableDeclarationName, DeclarationTyped};
use crate::error_type::CompilerError;
use crate::operations::{get_type_operator_binding_power, ExprType};
use crate::parser;
use crate::small_vec::{SizeType, SmallVec};
use crate::tokenizer::{Token, TokenList, TokenType};

#[derive(Copy, Clone)]
pub union CustomDouble {
  value: f64,
  bits: u64
}
impl std::ops::Neg for CustomDouble {
  type Output = CustomDouble;
  fn neg(self) -> Self::Output {
    return CustomDouble { value: unsafe { -self.value } };
  }
}

impl Display for CustomDouble {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", unsafe { self.value })
  }
}
impl Debug for CustomDouble {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", unsafe { self.value })
  }
}
impl PartialEq for CustomDouble {
  fn eq(&self, other: &Self) -> bool {
    unsafe { self.value == other.value }
  }
}
impl Eq for CustomDouble {}
impl Hash for CustomDouble {
  fn hash<H: Hasher>(&self, state: &mut H) {
    unsafe { self.bits }.hash(state);
  }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct KeyValueMap {
  key: Type,
  value: Type,
}

#[derive(Debug)]
pub enum ObjectSquareBracketReturn {
  KVMap(KeyValueMap),
  ComputedProp(ASTNode),
  MappedType(Type)
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct TypeFunctionArgument {
  spread: bool,
  name: String,
  conditional: bool,
  typ: Option<Type>
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Type {
  // Primitives
  
  Any,

  Number,
  NumberLiteral(CustomDouble),

  String,
  StringLiteral(String),

  Boolean,
  BooleanLiteral(bool),

  Unknown,

  /// Refers to both `void` and `undefined` type, as they're equal
  Void,

  /// A union (eg. `string | number`)
  Union(SmallVec<Type>),
  
  /// A selection (eg. `string & number`)
  Intersection(SmallVec<Type>),

  /// Custom types
  Custom(String),

  /// Arguments (eg. `Record<string, number>`)
  WithArgs(Box<Type>, SmallVec<Type>),

  /// Tuples (eg. `[string, number]`)
  Tuple { inner_types: SmallVec<(Type, bool)> },

  /// Array type (eg. `number[]`)
  Array(Box<Type>),

  /// A typed object (dict)
  Object {
    key_value: SmallVec<KeyValueMap>,
    parts: SmallVec<DeclarationTyped>
  },

  /// A mapped object (eg. `{ [K in keyof T]: ... }`)
  /// Different from `Object` because mapped objects can't have specific keys
  Mapped {
    key_name: String,
    key_type: Box<Type>,
    value_type: Box<Type>,
  },

  /// Used for type guards `x is string`
  Guard(String, Box<Type>),

  /// A typed declaration (used for parsing inside arrow functions)
  ColonDeclaration {
    spread: bool,
    name: String,
    typ: Box<Type>,
    conditional: bool
  },

  /// An un-typed spread argument, used during parsing
  SpreadParameter {
    name: String
  },

  /// A function (eg. `let a: (x: number) => void;`)
  Function {
    generics: SmallVec<Type>,
    params: SmallVec<TypeFunctionArgument>,
    return_type: Box<Type>,
    is_constructor: bool
  },

  /// An index into a type (eg. `type[prop]`)
  Index {
    callee: Box<Type>,
    property: Box<Type>
  },

  /// A direct access into a type (eg. `type.prop`)
  DirectAccess {
    callee: Box<Type>,
    property: Box<Type>
  },

  /// Specifies that this type points to the inferred type of a real value
  TypeOf(Box<Type>),

  /// Specifies that this type is a key of the given type
  KeyOf(Box<Type>),

  /// Conditionals are in the shape `left extends right ? if_true : if_false`
  Conditional {
    cnd_left: Box<Type>,
    cnd_right: Box<Type>,
    if_true: Box<Type>,
    if_false: Box<Type>
  },

  /// A single `extends`, not inside a conditional!
  Extends(Box<Type>, Box<Type>),

  /// Infers the type at this position, naming it after this.
  Infer(String),

  /// A type marked as read-only
  Readonly(Box<Type>),
}

impl Type {
  /// Joins this type with another in a union `|` (combining mutably)
  pub fn union(&mut self, other: Type) {
    if let Type::Union(types) = other {
      // Split other type into its parts
      for typ in types {
        self.union(typ);
      }
      return;
    }

    match self {
      Type::Union(types) => {
        let mut hasher = DefaultHasher::new();

        // Get this type's hash
        other.hash(&mut hasher);
        let new_hash = hasher.finish();

        let mut found_type = false;
        for t in types.iter() {
          t.hash(&mut hasher);
          if hasher.finish() == new_hash {
            found_type = true;
            break;
          }
        }

        // If the hash wasn't found anywhere, add it to the vector
        if !found_type {
          types.push(other);
        }
      }
      _ => {
        if other != *self {
          let mut new_union = Type::Union(SmallVec::with_element(self.clone()));
          new_union.union(other);
          *self = new_union;
        }
      }
    }
  }

  /// Joins this type with another in an intersection `&` (combining mutably)
  pub fn intersection(&mut self, other: Type) {
    if let Type::Intersection(types) = other {
      // Split other type into its parts
      for typ in types {
        self.intersection(typ);
      }
      return;
    }

    match self {
      Type::Intersection(types) => {
        let mut hasher = DefaultHasher::new();

        // Get this type's hash
        other.hash(&mut hasher);
        let new_hash = hasher.finish();

        let mut found_type = false;
        for t in types.iter() {
          t.hash(&mut hasher);
          if hasher.finish() == new_hash {
            found_type = true;
            break;
          }
        }

        // If the hash wasn't found anywhere, add it to the vector
        if !found_type {
          types.push(other);
        }
      }
      _ => {
        if other != *self {
          *self = Type::Intersection(SmallVec::with_element(self.clone()));
          self.intersection(other);
        }
      }
    }
  }

  pub fn get_single_name(&self) -> String {
    match &self {
      Type::Any => { "any".to_string() },

      Type::Number => { "number".to_string() },

      Type::String => { "string".to_string() },
      Type::StringLiteral(string) => { string.clone() },

      Type::Boolean => { "boolean".to_string() },
      Type::BooleanLiteral(boolean) => {
        if *boolean {
          "true".to_string()
        } else {
          "false".to_string()
        }
      },

      Type::Unknown => { "unknown".to_string() },
      Type::Void => { "void".to_string() },

      Type::Custom(name) => { name.to_string() },

      Type::WithArgs(typ, _) => { typ.get_single_name() },

      Type::Array(typ) => { typ.get_single_name() + "[]" },

      _ => { panic!("`get_single_name()` not implemented for {:?}", self) }
    }
  }
}

impl Type {
  pub fn inner_count(&self) -> usize {
    match self {
      Type::Union(inner) => inner.len(),
      Type::Intersection(inner) => inner.len(),
      Type::Object { parts, .. } => parts.len(),
      _ => todo!("type.inner_count not implemented for `{:?}`", self)
    }
  }
}

/// Gets a type, regardless of whether it starts with `:` or not.
/// If it *does* start with a `:`, that gets consumed and the type is returned.
pub fn get_type(
  tokens: &mut TokenList
) -> Result<Type, CompilerError> {
  if tokens.peek_str() == ":" {
    tokens.skip_unchecked();
  }
  get_expression(tokens, 0)
}

/// Gets a type only if the next token is `:`.
/// Otherwise, returns None
pub fn try_get_type(
  tokens: &mut TokenList
) -> Result<Option<Type>, CompilerError> {
  if tokens.peek_str() != ":" {
    Ok(None)
  } else {
    tokens.skip_unchecked(); // Skip ":"
    Ok(Some(get_expression(tokens, 0)?))
  }
}

fn parse_infix(
  mut left: Type,
  tokens: &mut TokenList,
  precedence: u8
) -> Result<Type, CompilerError> {
  let conditional = tokens.try_skip_and_ignore_whitespace("?");
  let infix_opr = tokens.consume();
  match infix_opr.value {
    "|" => {
      left.union(get_expression(tokens, precedence)?);
      Ok(left)
    }
    "&" => {
      left.intersection(get_expression(tokens, precedence)?);
      Ok(left)
    }
    "is" => {
      // Type guard
      tokens.skip_unchecked();
      tokens.ignore_whitespace();
      Ok(Type::Guard(
        // The left side of a type guard is a variable from the scope!
        left.get_single_name(),
        Box::new(get_expression(tokens, precedence)?)
      ))
    }
    ":" => {
      tokens.skip_unchecked();
      tokens.ignore_whitespace();
      let typ = get_expression(tokens, precedence)?;
      Ok(Type::ColonDeclaration {
        spread: false,
        name: left.get_single_name(),
        typ: Box::new(typ),
        conditional
      })
    }
    "[" => {
      // Type indexing
      let mut arguments: SmallVec<Type> = SmallVec::new();
      tokens.ignore_whitespace();
      if tokens.peek_str() != "]" {
        loop {
          tokens.ignore_whitespace();
          arguments.push(get_expression(tokens, 0)?);
          if tokens.peek_str() != "," {
            break;
          }
          tokens.skip_unchecked(); // Skip comma
        }
      }
      tokens.skip("]")?;

      if arguments.is_empty() {
        // Normal array notation
        Ok(Type::Array(Box::new(left)))
      } else if arguments.len() == 1 {
        Ok(Type::Index {
          callee: Box::new(left),
          property: Box::new(arguments.pop().unwrap())
        })
      } else {
        Err(CompilerError::new(
          format!(
            "Type indices need exactly one argument, found {}",
            arguments.len()
          ),
          infix_opr, tokens
        ))
      }
    }
    "." => {
      // Direct type access
      tokens.ignore_whitespace();
      Ok(Type::DirectAccess {
        callee: Box::new(left),
        property: Box::new(Type::Custom(
          tokens.consume_type(TokenType::Identifier)?.value.to_owned()
        ))
      })
    }
    "<" => {
      Ok(Type::WithArgs(Box::new(left), get_generics(tokens)?))
    }
    "extends" => {
      // This could be a normal `extends` (like generics), or a conditional
      let extends_precedence: u8 = get_type_operator_binding_power(
        ExprType::Infx, "extends"
      ).unwrap().0;

      // Get the next type
      let right_type = get_expression(
        tokens,
        extends_precedence
      )?;

      tokens.ignore_whitespace();
      if tokens.peek_str() != "?" {
        // Not a conditional! Just a normal `extends`
        return Ok(Type::Extends(
          Box::new(left),
          Box::new(right_type)
        ));
      }
      tokens.skip_unchecked(); // Skip "?"

      let if_true = get_expression(tokens, extends_precedence)?;

      // Skip ":"
      tokens.ignore_whitespace();
      tokens.skip(":")?;

      let if_false = get_expression(tokens, extends_precedence)?;

      Ok(Type::Conditional {
        cnd_left: Box::new(left),
        cnd_right: Box::new(right_type),
        if_true: Box::new(if_true),
        if_false: Box::new(if_false)
      })
    }
    other => {
      Err(CompilerError::new(
        format!(
          "Unexpected infix in type {:?}",
          other
        ),
        infix_opr, tokens
      ))
    }
  }
}

fn parse_name(
  tokens: &mut TokenList
) -> Type {
  match tokens.consume().value {
    "any" => Type::Any,
    "number" => Type::Number,
    "string" => Type::String,
    "boolean" => Type::Boolean,
    "true" => Type::BooleanLiteral(true),
    "false" => Type::BooleanLiteral(false),
    "void" | "undefined" => Type::Void,
    other => { Type::Custom(other.to_string()) }
  }
}

fn types_into_params(
  types: SmallVec<Type>,
  tokens: &mut TokenList
) -> Result<SmallVec<TypeFunctionArgument>, CompilerError> {
  let mut params = SmallVec::with_capacity(types.len());
  for p in types.iter() {
    params.push(match p {
      Type::ColonDeclaration {
        spread, name, typ, conditional
      } => {
        TypeFunctionArgument {
          spread: *spread,
          name: name.clone(),
          conditional: *conditional,
          typ: Some((**typ).clone())
        }
      }
      Type::SpreadParameter { name } => {
        TypeFunctionArgument {
          spread: true,
          name: name.clone(),
          conditional: false,
          typ: None
        }
      }
      Type::Custom(name) => {
        TypeFunctionArgument {
          spread: false,
          name: name.clone(),
          conditional: false,
          typ: None
        }
      }
      other => {
        return Err(CompilerError::new(
          format!(
            "Expected type argument, found type {:?}",
            other
          ),
          Token::from(""), tokens
        ))
      }
    });
  }
  Ok(params)
}

fn parse_prefix(
  tokens: &mut TokenList,
  precedence: u8
) -> Result<Type, CompilerError> {
  let prefix_opr = tokens.consume();
  match prefix_opr.value {
    "{" => {
      // Curly brace types (Objects or Mapped types)
      parse_curly_braces(tokens, precedence)
    }
    "[" => {
      // Tuple
      let mut inner_types = SmallVec::new();
      tokens.ignore_commas();
      loop {
        let has_spread = tokens.peek_str() == "...";
        if has_spread { tokens.skip_unchecked(); }
        inner_types.push((get_expression(tokens, 0)?, has_spread));

        // End on brackets
        if tokens.peek_str() == "]" { break }
        tokens.ignore_commas();
      }
      tokens.skip("]")?;
      Ok(Type::Tuple { inner_types })
    }
    "(" => {
      // Parenthesized type!
      // This could be an arrow function, too

      let mut paren_contents = SmallVec::with_capacity(4);
      tokens.ignore_commas();
      loop {
        tokens.ignore_whitespace();
        if tokens.peek_str() == ")" { break }

        paren_contents.push(get_type(tokens)?);
        if !tokens.ignore_commas() { break }
      }
      tokens.skip_unchecked(); // Skip ")"
      tokens.ignore_whitespace();

      if tokens.peek_str() != "=>" && tokens.peek_str() != ":" {
        // Normal parenthesized type
        if paren_contents.is_empty() {
          // Empty parenthesis?
          Err(CompilerError::expected("=>", tokens.consume(), tokens))
        } else if paren_contents.len() == 1 {
          // Remove the type from inside!
          Ok(paren_contents.pop().unwrap())
        } else {
          Ok(Type::Function {
            generics: SmallVec::new(),
            params: types_into_params(paren_contents, tokens)?,
            return_type: Box::new(Type::Unknown),
            is_constructor: false
          })
        }
      } else {
        // Function
        let params = types_into_params(paren_contents, tokens)?;
        tokens.skip_unchecked();
        let return_type = get_expression(tokens, precedence)?;
        Ok(Type::Function {
          generics: SmallVec::new(),
          params,
          return_type: Box::new(return_type),
          is_constructor: false
        })
      }
    }
    "-" => {
      let next_token = tokens.peek().clone();
      let next_typ = get_expression(tokens, precedence)?;
      match next_typ {
        Type::NumberLiteral(lit) => {
          Ok(Type::NumberLiteral(-lit))
        }
        other => {
          return Err(CompilerError::new(
            format!("Expected number after \"-\", found {:?}", other),
            next_token, tokens
          ));
        }
      }
    }
    "new" => {
      // Makes the proceeding function a constructor
      let next_token = tokens.peek().clone();
      let mut next_type = get_expression(tokens, precedence)?;
      match &mut next_type {
        Type::Function { is_constructor, .. } => {
          *is_constructor = true;
        }
        other => {
          return Err(CompilerError::new(
            format!("`new` prefix expected \"Function\", found {:?}", other),
            next_token, tokens
          ))
        }
      }
      Ok(next_type)
    }
    "..." => {
      // Spread for arguments
      let mut param = get_expression(tokens, 0)?;
      match &mut param {
        Type::ColonDeclaration { spread, .. } => {
          *spread = true;
          Ok(param)
        }
        Type::Custom(name) => {
          Ok(Type::SpreadParameter { name: name.clone() })
        }
        _ => {
          Err(CompilerError::new(
            format!("Expected parameter after spread, found {:?}", param),
            Token::from(""), tokens
          ))
        }
      }
    }
    "|" | "&" => {
      // Prefix symbols used as syntactic sugar are ignored
      get_expression(tokens, precedence)
    }
    "<" => {
      // Start of function with generics
      let mut generics = get_generics(tokens)?;
      let next_token = tokens.peek().clone();
      let mut next_fn = get_expression(tokens, precedence)?;
      match &mut next_fn {
        Type::Function { generics: inner_generics, .. } => {
          inner_generics.append(&mut generics);
        }
        other => {
          return Err(CompilerError::new(
            format!("Expected Function type, found {:?}", other),
            next_token, tokens
          ))
        }
      }
      Ok(next_fn)
    }
    "asserts" | "unique" | "abstract" => {
      // These types are ignored
      get_expression(tokens, precedence)
    }
    "typeof" => {
      Ok(Type::TypeOf(Box::new(get_expression(tokens, precedence)?)))
    }
    "keyof" => {
      Ok(Type::KeyOf(Box::new(get_expression(tokens, precedence)?)))
    }
    "infer" => {
      tokens.ignore_whitespace();
      Ok(Type::Infer(tokens.consume().value.to_string()))
    }
    "readonly" => {
      Ok(Type::Readonly(
        Box::new(get_expression(tokens, precedence)?)
      ))
    }
    other => {
      Err(CompilerError::new(
        format!(
          "Type prefix operator not found: {:?}",
          other
        ),
        prefix_opr, tokens
      ))
    }
  }
}

fn parse_curly_braces(
  tokens: &mut TokenList,
  precedence: u8
) -> Result<Type, CompilerError> {
  let mut obj_parts = SmallVec::new();
  let mut kv_maps = SmallVec::new();
  let mut mapped_type: Option<Type> = None;

  tokens.ignore_commas();
  loop {
    tokens.ignore_whitespace();
    if tokens.peek_str() == "}" {
      tokens.skip_unchecked();
      break;
    }
    
    let property = if tokens.peek_str() == "[" {
      let osbr = parse_object_square_bracket(tokens)?;
      match osbr {
        ObjectSquareBracketReturn::KVMap(kv_map) => {
          kv_maps.push(kv_map);
          continue;
        }
        ObjectSquareBracketReturn::ComputedProp(value) => {
          ComputableDeclarationName::new_computed(value)
        }
        ObjectSquareBracketReturn::MappedType(typ) => {
          if mapped_type.is_some() {
            return Err(CompilerError::new(
              "Can't have multiple mapped types in one object.".to_string(),
              tokens.consume(), tokens
            ));
          }
          mapped_type = Some(typ.clone());
          continue;
        }
      }
    } else {
      ComputableDeclarationName::new_named(tokens.consume().value.to_string())
    };
    
    // Get property type (fallback to `any`)
    tokens.ignore_whitespace();
    let property_type = if tokens.peek_str() == ":" {
      tokens.skip_unchecked(); // Skip ":"
      get_expression(tokens, *crate::operations::COMMA_PRECEDENCE)?
    } else {
      Type::Any
    };

    // Push
    obj_parts.push(DeclarationTyped::from_parts(property, property_type));

    // Ignore commas / exit
    tokens.ignore_commas();
  }

  if mapped_type .is_some() {
    return Ok(mapped_type.unwrap());
  }

  Ok(Type::Object {
    key_value: kv_maps,
    parts: obj_parts,
  })
}

pub fn parse_object_square_bracket(
  tokens: &mut TokenList
) -> Result<ObjectSquareBracketReturn, CompilerError> {
  tokens.skip("[")?;
  tokens.ignore_whitespace();

  let checkpoint = tokens.get_checkpoint();
  
  // Although this is usually "key", it can be any identifier!
  let key_token = tokens.consume_type(TokenType::Identifier)?;
  tokens.ignore_whitespace();

  if tokens.peek_str() == "in" {
    tokens.ignore_checkpoint(checkpoint);
    return get_kvc_complex_key(key_token, tokens);
  }

  tokens.ignore_whitespace();
  if tokens.peek_str() != ":" {
    // It's computed!
    tokens.restore_checkpoint(checkpoint);
    let computed = parser::get_expression_without_symbol_table(tokens, 0)?;
    tokens.skip("]")?;
    return Ok(
      ObjectSquareBracketReturn::ComputedProp(computed)
    )
  } else {
    tokens.ignore_checkpoint(checkpoint);
  }
  tokens.skip(":")?;

  let key_type = get_expression(tokens, 0)?;

  tokens.ignore_whitespace();
  tokens.skip("]")?;

  tokens.ignore_whitespace();
  let is_optional = tokens.try_skip_and_ignore_whitespace("?");
  tokens.skip(":")?;

  let mut value_type = get_expression(tokens, 0)?;
  if is_optional {
    // Make value type also include `undefined`
    value_type.union(Type::Void);
  }

  Ok(ObjectSquareBracketReturn::KVMap(
    KeyValueMap { key: key_type, value: value_type }
  ))
}

fn get_kvc_complex_key(
  key_token: Token,
  tokens: &mut TokenList
) -> Result<ObjectSquareBracketReturn, CompilerError> {
  let key_name = key_token.value.to_string();
  tokens.skip("in")?;
  tokens.ignore_whitespace();
  let key_type = get_expression(tokens, 0)?;
  tokens.ignore_whitespace();
  tokens.skip("]")?;
  tokens.ignore_whitespace();
  let is_optional = tokens.try_skip_and_ignore_whitespace("?");
  tokens.skip(":")?;
  let mut value_type = get_expression(tokens, 0)?;
  if is_optional {
    // Make value type also include `undefined`
    value_type.union(Type::Void);
  }
  return Ok(ObjectSquareBracketReturn::MappedType(
    Type::Mapped {
      key_name,
      key_type: Box::new(key_type),
      value_type: Box::new(value_type)
    }
  ));
}

fn get_expression(
  tokens: &mut TokenList,
  precedence: u8
) -> Result<Type, CompilerError> {
  tokens.ignore_whitespace();

  let mut left = {
    let next = tokens.peek();
    // println!("Handling prefix {:?}", next);

    if [ ")", "]", "}", ",", ";" ].contains(&next.value) {
      // End it here!
      return Ok(Type::Unknown);
    }

    // let next = tokens.peek();
    match &next.typ {
      TokenType::Number => {
        let value: f64 = tokens.consume().value.parse().unwrap();
        Type::NumberLiteral(CustomDouble {
          value
        })
      }
      TokenType::String => {
        Type::StringLiteral(tokens.consume().value.to_string())
      }
      TokenType::Identifier | TokenType::Symbol => {
        let binding_power = get_type_operator_binding_power(
          ExprType::Prefx,
          next.value
        );
        if let Some(binding_power) = binding_power {
          // Prefix operators
          parse_prefix(tokens, binding_power.1)?
        } else if next.typ == TokenType::Identifier {
          // Could be a name...
          parse_name(tokens)
        } else {
          return Err(CompilerError::new(
            format!(
              "Prefix operator not found when fetching type: {:?}",
              next
            ),
            tokens.consume(), tokens
          ))
        }
      }
      other => {
        return Err(CompilerError::new(
          format!(
            "Unexpected token when parsing type: {:?}",
            other
          ),
          tokens.consume(), tokens
        ));
      }
    }
  };

  loop {
    // Get the next token
    tokens.ignore_whitespace();
    let next = tokens.peek();
    if next.typ == TokenType::EndOfFile { break; }

    // println!(
    //   "Handling infix/postfix (w/ min precedence {}) in type: {:?}",
    //   precedence, next
    // );

    // Handle infix
    let binding_power = if let Some(binding_power) = get_type_operator_binding_power(
      ExprType::Infx, next.value
    ) {
      if binding_power.0 < precedence {
        break;
      }
      binding_power
    } else {
      // No infix for this symbol!
      break;
    };

    left = parse_infix(left, tokens, binding_power.1)?;
  }

  Ok(left)
}

/// Gets types separated by commas
pub fn get_comma_separated_types_until(
  tokens: &mut TokenList,
  until_str: &[&str]
) -> Result<SmallVec<Type>, CompilerError> {
  let mut types = SmallVec::new();
  tokens.ignore_commas();
  loop {
    if until_str.contains(&tokens.peek_str()) {
      break
    }
    types.push(get_type(tokens)?);
    if tokens.peek_str() == "=" {
      // Type defaults are ignored! They're an artifact of TypeScript's
      // older type inference, which couldn't infer a lot of the more
      // complex types.
      tokens.skip_unchecked();
      get_type(tokens)?;
    }
    if !tokens.ignore_commas() { break }
  }
  Ok(types)
}

/// Gets generics if available, otherwise returns an empty vec
pub fn get_optional_generics(
  tokens: &mut TokenList
) -> Result<SmallVec<Type>, CompilerError> {
  // Get generics
  if tokens.peek_str() != "<" { return Ok(SmallVec::new()); }
  tokens.skip_unchecked(); // Skip "<"
  get_generics(tokens)
}

/// Gets generics until it finds a ">", consuming it.
/// Used after the first "<", as it will not consume it!
pub fn get_generics(
  tokens: &mut TokenList
) -> Result<SmallVec<Type>, CompilerError> {
  let generics = get_comma_separated_types_until(
    tokens, &[ ">", ")", ";" ]
  )?;
  if !tokens.peek_str().starts_with(">") {
    return Err(CompilerError::expected(
      ">", tokens.consume(), tokens
    ));
  }
  if tokens.peek_str().len() > 1 {
    let _ = tokens.consume_single_character();
  } else {
    tokens.skip_unchecked(); // Skip ">"
  }
  Ok(generics)
}
