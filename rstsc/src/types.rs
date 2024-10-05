use std::collections::hash_map::DefaultHasher;
use std::fmt::{Display, Debug};
use std::hash::{Hash, Hasher};

use crate::error_type::CompilerError;
use crate::operations::{get_type_operator_binding_power, ExprType};
use crate::parser::INVERSE_GROUPINGS;
use crate::tokenizer::{Token, TokenList, TokenType};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct TypedNamedDeclaration {
  pub name: String,
  pub typ: Type,
  pub computed: bool
}

#[derive(Copy, Clone)]
pub union CustomDouble {
  value: f64,
  bits: u64
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

pub enum KVMapOrComputedProp {
  KVMap(KeyValueMap),
  ComputedProp(String)
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
  Union(Vec<Type>),
  
  /// A selection (eg. `string & number`)
  Intersection(Vec<Type>),

  /// Custom types
  Custom(String),

  /// Arguments (eg. `Record<string, number>`)
  WithArgs(Box<Type>, Vec<Type>),

  /// Tuples (eg. `[string, number]`)
  Tuple { inner_types: Vec<Type>, spread_idx: usize },

  /// Array type (eg. `number[]`)
  Array(Box<Type>),

  /// A typed object (dict)
  Object {
    key_value: Option<Box<KeyValueMap>>,
    parts: Vec<TypedNamedDeclaration>
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
    params: Vec<TypeFunctionArgument>,
    return_type: Box<Type>,
    is_constructor: bool
  },

  /// An index into a type (eg. `type[prop]`)
  Index {
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
    match other {
      Type::Union(types) => {
        // Split other type into its parts
        for typ in types {
          self.union(typ);
        }
        return;
      },
      _ => {}
    }

    match self {
      Type::Union(types) => {
        let mut hasher = DefaultHasher::new();

        // Get this type's hash
        other.hash(&mut hasher);
        let new_hash = hasher.finish();

        let mut found_type = false;
        for t in types.into_iter() {
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
          let mut new_union = Type::Union(vec![ self.clone() ]);
          new_union.union(other);
          *self = new_union;
        }
      }
    }
  }

  /// Joins this type with another in an intersection `&` (combining mutably)
  pub fn intersection(&mut self, other: Type) {
    match other {
      Type::Intersection(types) => {
        // Split other type into its parts
        for typ in types {
          self.intersection(typ);
        }
        return;
      },
      _ => {}
    }

    match self {
      Type::Intersection(types) => {
        let mut hasher = DefaultHasher::new();

        // Get this type's hash
        other.hash(&mut hasher);
        let new_hash = hasher.finish();

        let mut found_type = false;
        for t in types.into_iter() {
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
          let mut new_union = Type::Intersection(vec![ self.clone() ]);
          new_union.intersection(other);
          *self = new_union;
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

      Type::Array(typ) => { typ.to_string() },

      _ => { "".to_string() }
    }
  }
}

impl Display for Type {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Type::Any => { f.write_str("any") },

      Type::Number => { f.write_str("number") },
      Type::NumberLiteral(number) => {
        f.write_str(&unsafe { number.value }.to_string())
      },

      Type::String => { f.write_str("string") },
      Type::StringLiteral(string) => {
        f.write_str(string)
      },

      Type::Boolean => { f.write_str("boolean") },
      Type::BooleanLiteral(boolean) => {
        f.write_str(if *boolean {
          "true"
        } else {
          "false"
        })
      },

      Type::Unknown => { f.write_str("unknown") },
      Type::Void => { f.write_str("void") },

      Type::Union(types) => {
        f.write_str(
          &types.iter()
            .map(|t| t.to_string()).collect::<Vec<String>>()
            .join(" | ")
        )
      },
      Type::Intersection(types) => {
        f.write_str(
          &types.iter()
            .map(|t| t.to_string()).collect::<Vec<String>>()
            .join(" & ")
        )
      },

      Type::Custom(name) => { f.write_str(&name) },

      Type::WithArgs(typ, args) => {
        f.write_str(&typ.to_string())?;
        f.write_str(
          &args.iter()
            .map(|t| t.to_string()).collect::<Vec<String>>()
            .join(" | ")
        )
      },

      Type::Tuple { inner_types, spread_idx } => {
        let spread_idx = *spread_idx;
        f.write_str("[ ")?;
        f.write_str(
          &inner_types.iter().enumerate()
            .map(|(index, typ)| {
              if spread_idx == index {
                "...".to_owned() + &typ.to_string()
              } else {
                typ.to_string()
              }
            }).collect::<Vec<String>>()
            .join(", ")
        )?;
        f.write_str(" ]")
      },

      Type::Array(typ) => {
        f.write_str(&typ.to_string())?;
        f.write_str("[]")
      },

      Type::Object{ key_value, parts } => {
        f.write_str("{ ")?;
        if let Some(key_value) = key_value {
          f.write_str("[key: ")?;
          std::fmt::Display::fmt(&key_value.key, f)?;
          f.write_str("]: ")?;
          std::fmt::Display::fmt(&key_value.value, f)?;
          f.write_str(", ")?;
        }
        f.write_str(
          &parts.iter()
            .map(|kv| {
              kv.name.clone() + " " + &kv.typ.to_string()
            }).collect::<Vec<String>>()
            .join(", ")
        )?;
        f.write_str(" }")
      },

      Type::Guard(name, typ) => {
        f.write_str(name)?;
        f.write_str(" is ")?;
        f.write_str(&typ.to_string())
      },

      Type::ColonDeclaration {
        spread,
        name,
        typ,
        conditional
      } => {
        if *spread { f.write_str("...")?; }
        f.write_str(name)?;
        if *conditional { f.write_str("?")?; }
        f.write_str(": ")?;
        f.write_str(&typ.to_string())
      },

      Type::SpreadParameter { name } => {
        f.write_str("...")?;
        f.write_str(name)
      },

      Type::Function {
        params,
        return_type,
        is_constructor
      } => {
        if *is_constructor { f.write_str("new ")?; }
        f.write_str("(")?;
        let mut remaining = params.len();
        for param in params {
          if param.spread { f.write_str("...")?; }
          f.write_str(&param.name)?;
          if param.conditional { f.write_str("?")?; }
          if let Some(typ) = &param.typ {
            f.write_str(": ")?;
            f.write_str(&typ.to_string())?;
          }
          remaining -= 1;
          if remaining > 0 { f.write_str(", ")?; }
        }
        f.write_str(") => ")?;
        f.write_str(&return_type.to_string())
      }
      Type::Index { callee, property } => {
        f.write_str(&callee.to_string())?;
        f.write_str("[")?;
        f.write_str(&property.to_string())?;
        f.write_str("]")
      }
      Type::TypeOf(inner) => {
        f.write_str("typeof ")?;
        std::fmt::Display::fmt(&inner, f)
      }
      Type::KeyOf(inner) => {
        f.write_str("keyof ")?;
        std::fmt::Display::fmt(&inner, f)
      }
      Type::Conditional {
        cnd_left, cnd_right,
        if_true, if_false
      } => {
        f.write_str(&cnd_left.to_string())?;
        f.write_str(" extends ")?;
        f.write_str(&cnd_right.to_string())?;
        f.write_str(" ? ")?;
        f.write_str(&if_true.to_string())?;
        f.write_str(" : ")?;
        f.write_str(&if_false.to_string())
      }
      Type::Extends(left, right) => {
        f.write_str(&left.to_string())?;
        f.write_str(" extends ")?;
        f.write_str(&right.to_string())
      }
      Type::Infer(name) => {
        f.write_str("infer ")?;
        f.write_str(name)
      }
      Type::Readonly(inner) => {
        f.write_str("readonly ")?;
        std::fmt::Display::fmt(inner, f)
      }
    }
  }
}

impl Type {
  pub fn inner_count(&self) -> usize {
    match self {
      Type::Union(inner) => inner.len(),
      Type::Intersection(inner) => inner.len(),
      Type::Object { parts, .. } => parts.len(),
      _ => todo!("type.inner_count not implemented for `{}`", self)
    }
  }
}

/// Gets a type, regardless of whether it starts with `:` or not.
#[must_use]
pub fn get_type<'a, 'b>(
  tokens: &'b mut TokenList<'a>
) -> Result<Type, CompilerError<'a>> where 'a: 'b {
  if tokens.peek_str() == ":" {
    tokens.skip_unchecked();
  }

  return get_expression(tokens, 0);
}

/// Gets a type only if the next token is `:`.
/// Otherwise, returns None
#[must_use]
pub fn try_get_type<'a, 'b>(
  tokens: &'b mut TokenList<'a>
) -> Result<Option<Type>, CompilerError<'a>> where 'a: 'b {
  if tokens.peek_str() != ":" {
    Ok(None)
  } else {
    tokens.skip_unchecked(); // Skip ":"
    Ok(Some(get_expression(tokens, 0)?))
  }
}

pub fn get_key_value_or_computed_property<'a, 'b>(
  tokens: &'b mut TokenList<'a>
) -> Result<KVMapOrComputedProp, CompilerError<'a>> where 'a: 'b {
  if tokens.peek_str() != "[" {
    return Err(CompilerError {
      message: "Expected key-value type!".to_string(),
      token: tokens.consume()
    });
  }

  tokens.skip_unchecked(); // Skip "["
  tokens.ignore_whitespace();
  
  // Although this is usually "key", it can be any identifier!
  let key_token = tokens.consume();
  if !key_token.is_identifier() {
    return Err(CompilerError {
      message: "Expected identifier for \"[key: type]: type\" declaration".to_string(),
      token: key_token
    });
  }

  tokens.ignore_whitespace();
  if tokens.peek_str() == "]" {
    tokens.skip_unchecked(); // Skip "]"
    return Ok(
      KVMapOrComputedProp::ComputedProp(key_token.value.to_owned())
    )
  }
  tokens.skip(":")?;

  let key_type = get_expression(tokens, 0)?;

  tokens.ignore_whitespace();
  tokens.skip("]")?;

  tokens.ignore_whitespace();
  tokens.skip(":")?;

  let value_type = get_expression(tokens, 0)?;

  Ok(KVMapOrComputedProp::KVMap(
    KeyValueMap { key: key_type, value: value_type }
  ))
}

fn parse_infix<'a, 'b>(
  mut left: Type,
  tokens: &'b mut TokenList<'a>,
  precedence: u8
) -> Result<Type, CompilerError<'a>> where 'a: 'b {
  let conditional = if tokens.peek_str() == "?" {
    tokens.skip_unchecked();
    true
  } else { false };
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
        left.to_string(),
        Box::new(get_expression(tokens, precedence)?)
      ))
    }
    ":" => {
      tokens.skip_unchecked();
      tokens.ignore_whitespace();
      let typ = get_expression(tokens, precedence)?;
      Ok(Type::ColonDeclaration {
        spread: false,
        name: left.to_string(),
        typ: Box::new(typ),
        conditional
      })
    }
    "[" | "<" => {
      // Type indexing and generics
      let group_start = infix_opr.value.to_string();
      let group_end = INVERSE_GROUPINGS[infix_opr.value];

      let mut arguments: Vec<Type> = vec![];
      tokens.ignore_whitespace();
      if tokens.peek_str() != group_end {
        loop {
          tokens.ignore_whitespace();
          arguments.push(get_expression(tokens, 0)?);
          if tokens.peek_str() != "," {
            break;
          }
          tokens.skip_unchecked(); // Skip comma
        }
      }

      if group_start == "<" && tokens.peek_str().len() != 1 {
        // Some bunched-up closing brackets (eg. `>>`)
        // This happens due to bit-shifting using this token.

        let first_char = tokens.consume_single_character();
        if first_char == ">" {
          // Skipped!
        } else {
          // Wrong character!
          return Err(CompilerError::expected("<", Token::from(first_char)));
        }
      } else {
        tokens.skip(group_end)?;
      }

      if group_start == "[" {
        if arguments.len() == 0 {
          // Normal array notation
          Ok(Type::Array(Box::new(left)))
        } else if arguments.len() == 1 {
          Ok(Type::Index {
            callee: Box::new(left),
            property: Box::new(arguments.pop().unwrap())
          })
        } else {
          Err(CompilerError {
            message: format!(
              "Type indices need exactly one argument, found {}",
              arguments.len()
            ),
            token: infix_opr
          })
        }
      } else {
        // Generic arguments
        Ok(Type::WithArgs(Box::new(left), arguments))
      }
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
      Err(CompilerError {
        message: format!(
          "Unexpected infix in type {:?}",
          other
        ),
        token: infix_opr
      })
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
    "void" => Type::Void,
    other => { Type::Custom(other.to_string()) }
  }
}

fn parse_prefix<'a, 'b>(
  tokens: &'b mut TokenList<'a>,
  precedence: u8
) -> Result<Type, CompilerError<'a>> where 'a: 'b {
  let prefix_opr = tokens.consume();
  match prefix_opr.value {
    "{" => {
      // Dictionary object
      let mut obj_parts: Vec<TypedNamedDeclaration> = vec![];
      let mut kv_type = None;
      loop {
        // TODO: Handle `[key: string]: number`

        tokens.ignore_whitespace();
        let mut is_computed_property = false;
        let mut computed_name = None;
        if tokens.peek_str() == "}" {
          tokens.skip_unchecked();
          break;
        } else if tokens.peek_str() == "[" {
          let kv_or_cp = get_key_value_or_computed_property(tokens)?;
          match kv_or_cp {
            KVMapOrComputedProp::KVMap(kv_map) => {
              kv_type = Some(kv_map);
              continue;
            }
            KVMapOrComputedProp::ComputedProp(name) => {
              computed_name = Some(name);
              is_computed_property = true;
            }
          }
        }

        // Get property name
        let property = if let Some(property_name) = computed_name {
          property_name
        } else {
          tokens.consume().value.to_string()
        };
        
        // Get property type (fallback to `any`)
        tokens.ignore_whitespace();
        let property_type = if tokens.peek_str() == ":" {
          tokens.skip_unchecked(); // Skip ":"
          get_expression(tokens, precedence)?
        } else {
          Type::Any
        };

        // Push
        obj_parts.push(TypedNamedDeclaration {
          name: property,
          typ: property_type,
          computed: is_computed_property
        });

        // Ignore commas / exit
        tokens.ignore_whitespace();
        while tokens.peek_str() == "," {
          tokens.skip_unchecked();
        }
      }

      Ok(Type::Object {
        key_value: kv_type.map(Box::new),
        parts: obj_parts,
      })
    }
    "[" => {
      // Tuple
      let mut inner_types: Vec<Type> = vec![];
      let mut spread_idx = usize::MAX;
      loop {
        if tokens.peek_str() == "..." {
          // Get spread
          if spread_idx != usize::MAX {
            // Multiple spreads are not allowed
            // eg. `let a: [boolean, ...number[], ...string[]]`
            return Err(CompilerError {
              message: "Can't have multiple spread elements in one tuple.".to_string(),
              token: tokens.consume()
            });
          }
          spread_idx = inner_types.len();
          tokens.skip_unchecked(); // Skip "..."
        }
        inner_types.push(get_expression(tokens, precedence)?);

        // Ignore commas
        tokens.ignore_whitespace();
        while tokens.peek_str() == "," {
          tokens.skip_unchecked();
          tokens.ignore_whitespace();
        }

        // End on brackets
        if tokens.peek_str() == "]" {
          tokens.skip_unchecked();
          break;
        }
      }
      Ok(Type::Tuple { inner_types, spread_idx })
    }
    "(" => {
      // Parenthesized type!
      // This could be an arrow function, too

      let mut params_as_types = vec![];
      loop {
        tokens.ignore_whitespace();
        if tokens.peek_str() == "," {
          tokens.skip_unchecked();
        } else if tokens.peek_str() == ")" {
          tokens.skip_unchecked();
          break;
        }

        params_as_types.push(get_type(tokens)?);
      }
      let mut params = vec![];
      for p in params_as_types.iter() {
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
          other if params.len() > 0 => {
            return Err(CompilerError {
              message: format!(
                "Expected type argument, found type {:?}",
                other
              ),
              token: Token::from("")
            })
          }
          _ => {
            // Not an arrow function
            break;
          }
        });
      }

      tokens.ignore_whitespace();
      if tokens.peek_str() == "=>" || tokens.peek_str() == ":" {
        // Function
        tokens.skip_unchecked();
        let return_type = get_expression(tokens, precedence)?;
        Ok(Type::Function {
          params,
          return_type: Box::new(return_type),
          is_constructor: false
        })
      } else if params.len() != 1 {
        Ok(Type::Function {
          params,
          return_type: Box::new(Type::Unknown),
          is_constructor: false
        })
      } else {
        // Simple parenthesis, remove the type from inside
        Ok(params_as_types.pop().unwrap())
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
          return Err(CompilerError {
            message: format!("`new` prefix expected \"Function\", found {:?}", other),
            token: next_token
          })
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
          Err(CompilerError {
            message: format!("Expected parameter after spread, found {:?}", param),
            token: Token::from("")
          })
        }
      }
    }
    "|" | "&" => {
      // Prefix symbols used as syntactic sugar are ignored
      get_expression(tokens, precedence)
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
      Err(CompilerError {
        message: format!(
          "Prefix operator not found: {:?}",
          other
        ),
        token: prefix_opr
      })
    }
  }
}

fn get_expression<'a, 'b>(
  tokens: &'b mut TokenList<'a>,
  precedence: u8
) -> Result<Type, CompilerError<'a>> where 'a: 'b {
  tokens.ignore_whitespace();

  let mut left = {
    let next = tokens.peek();
    // println!("Handling prefix {:?}", next);

    if [ ")", "]", "}", ",", ";" ].contains(&next.value) {
      // End it here!
      return Ok(Type::Unknown);
    }

    let next = tokens.peek();
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
        } else if next.is_identifier() {
          // Could be a name...
          parse_name(tokens)
        } else {
          return Err(CompilerError {
            message: format!(
              "Prefix operator not found when fetching type: {:?}",
              next
            ),
            token: tokens.consume()
          })
        }
      }
      other => {
        return Err(CompilerError {
          message: format!(
            "Unexpected token when parsing type: {:?}",
            other
          ),
          token: tokens.consume()
        });
      }
    }
  };

  loop {
    // Get the next token
    tokens.ignore_whitespace();
    let next = tokens.peek();
    if next.typ == TokenType::EndOfFile { break; }

    // println!(
    //     "Handling infix/postfix (w/ min precedence {}) in type: {:?}",
    //     precedence, next
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
