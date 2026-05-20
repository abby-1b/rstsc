use std::fmt::{Debug, Display};
use std::hash::{Hash, Hasher};

use crate::ast::ASTIndex;
use crate::declaration::{ComputableDeclarationName, DeclarationTyped};
use crate::error_type::CompilerError;
use crate::operations::{get_type_operator_binding_power, ExprType};
use crate::parser;
use crate::small_vec::SmallVec;
use crate::source_properties::{SourceProperties, SrcMapping};
use crate::tokenizer::{Token, TokenType};
use crate::type_arena::TypeIndex;

#[derive(Copy, Clone)]
pub union CustomDouble {
  pub value: f64,
  pub bits: u64,
}
impl CustomDouble {
  pub fn from_str(value: &str) -> Result<Self, ()> {
    if let Ok(value) = value.parse() {
      Ok(CustomDouble { value })
    } else {
      Err(())
    }
  }
}
impl std::ops::Neg for CustomDouble {
  type Output = CustomDouble;
  fn neg(self) -> Self::Output {
    return CustomDouble {
      value: unsafe { -self.value },
    };
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

#[derive(Debug, Clone)]
pub struct KeyValueMap {
  key: TypeIndex,
  value: TypeIndex,
}

#[derive(Debug)]
pub enum ObjectSquareBracketReturn {
  KVMap(KeyValueMap),
  ComputedProp(ASTIndex),
  MappedType(TypeIndex),
}

#[derive(Debug, Clone)]
pub struct TypeFunctionArgument {
  spread: bool,
  name: String,
  conditional: bool,
  typ: Option<TypeIndex>,
}

#[derive(Debug, Clone)]
pub enum Type {
  // Primitives
  Any,

  Number,
  NumberLiteral(CustomDouble),
  BigInt,
  BigIntLiteral(CustomDouble),

  String,
  StringLiteral(String),

  Boolean,
  BooleanLiteral(bool),

  RegExp,

  Unknown,

  /// Refers to both `void` and `undefined` type, as they're equal
  Void,

  /// A union (eg. `string | number`)
  Union(SmallVec<TypeIndex>),

  /// An intersection (eg. `{ a: string } & { b: number }` => `{ a: string, b: number }`)
  Intersection(SmallVec<TypeIndex>),

  /// Custom types
  Custom(String),

  /// Arguments (eg. `Record<string, number>`)
  WithArgs(TypeIndex, SmallVec<TypeIndex>),

  /// Tuples (eg. `[string, number]`)
  /// Stored as (type, has_spread)
  Tuple {
    inner_types: SmallVec<(TypeIndex, bool)>,
  },

  /// Array type (eg. `number[]`)
  Array(TypeIndex),

  /// A typed object (dict)
  Object {
    key_value: SmallVec<KeyValueMap>,
    parts: SmallVec<DeclarationTyped>,
  },

  /// A mapped object (eg. `{ [K in keyof T]: ... }`)
  /// Different from `Object` because mapped objects can't have specific keys
  Mapped {
    key_name: String,
    key_type: TypeIndex,
    value_type: TypeIndex,
  },

  /// Used for type guards `x is string`
  Guard(String, TypeIndex),

  /// A typed declaration (used for parsing inside arrow functions)
  ColonDeclaration {
    spread: bool,
    name: String,
    typ: TypeIndex,
    conditional: bool,
  },

  /// An un-typed spread argument, used during parsing
  SpreadParameter {
    name: String,
  },

  /// A function (eg. `let a: (x: number) => void;`)
  Function {
    generics: SmallVec<TypeIndex>,
    params: SmallVec<TypeFunctionArgument>,
    return_type: TypeIndex,
    is_constructor: bool,
  },

  /// An index into a type (eg. `type[prop]`)
  Index {
    callee: TypeIndex,
    property: TypeIndex,
  },

  /// A direct access into a type (eg. `type.prop`)
  DirectAccess {
    callee: TypeIndex,
    property: TypeIndex,
  },

  /// Specifies that this type points to the inferred type of a real value
  TypeOf(TypeIndex),

  /// Specifies that this type is a key of the given type
  KeyOf(TypeIndex),

  /// Conditionals are in the shape `left extends right ? if_true : if_false`
  Conditional {
    cnd_left: TypeIndex,
    cnd_right: TypeIndex,
    if_true: TypeIndex,
    if_false: TypeIndex,
  },

  /// A single `extends`, not inside a conditional!
  Extends(TypeIndex, TypeIndex),

  /// Infers the type at this position, naming it after this.
  Infer(String),

  /// A type marked as read-only
  Readonly(TypeIndex),
}

impl Type {
  /// Checks if a type matches another. Order matters!
  /// e.g. `123` matches `number`, but not the other way around.
  pub fn matches(specific_type: TypeIndex, broad_type: TypeIndex, sp: &SourceProperties) -> bool {
    use Type::*;
    if specific_type == broad_type {
      return true;
    }
    match (sp.types.get(specific_type), sp.types.get(broad_type)) {
      (Any, _) | (_, Any) => true,
      (NumberLiteral(..), Number) => true,
      (StringLiteral(..), String) => true,
      (BooleanLiteral(..), Boolean) => true,
      (Union(many_specific), _broad_union @ Union(..)) => {
        for s in many_specific {
          if !Self::matches(*s, broad_type, sp) {
            return false;
          }
        }
        true
      }
      (_one, Union(many)) => {
        for m in many {
          if Type::matches(specific_type, *m, sp) {
            return true;
          }
        }
        false
      }

      _ => false,
    }
  }

  /// Joins this type with another in a union `|` (combining mutably)
  pub fn union(target_idx: TypeIndex, other_idx: TypeIndex, sp: &mut SourceProperties) {
    let other = sp.types.get(other_idx);

    // `other` is already a union (split it)
    if matches!(sp.types.get(other_idx), Type::Union(..)) {
      let other_indices = match sp.types.get(other_idx) {
        Type::Union(indices) => indices.clone(),
        _ => unreachable!(),
      };
      for &typ_idx in &other_indices {
        Self::union(target_idx, typ_idx, sp);
      }
      return;
    }

    // `target` is already a union (add to it)
    if matches!(sp.types.get(target_idx), Type::Union(..)) {
      let target_indices = match sp.types.get(target_idx) {
        Type::Union(indices) => indices.clone(),
        _ => unreachable!(),
      };

      let mut found_type = false;
      for t in target_indices.iter() {
        if sp.types.get(*t) == other {
          found_type = true;
          break;
        }
      }

      if !found_type {
        match sp.types.get_mut(target_idx) {
          Type::Union(indices) => indices.push(other_idx),
          _ => unreachable!(),
        }
      }
      return;
    }

    // Neither is a union, so make one!
    if sp.types.get(target_idx) != sp.types.get(other_idx) {
      let old_type = sp.types.get(target_idx).clone();
      let old_type_idx = sp.types.add(old_type);

      let mut indices = SmallVec::with_element(old_type_idx);
      indices.push(other_idx);

      let new_union = Type::Union(indices);
      *sp.types.get_mut(target_idx) = new_union;
    }
  }

  /// Joins this type with another in an intersection `&` (combining mutably)
  pub fn intersection(target_idx: TypeIndex, other_idx: TypeIndex, sp: &mut SourceProperties) {
    let other = sp.types.get(other_idx);

    // `other` is already an intersection (split it)
    if matches!(sp.types.get(other_idx), Type::Intersection(..)) {
      let other_indices = match sp.types.get(other_idx) {
        Type::Intersection(indices) => indices.clone(),
        _ => unreachable!(),
      };
      for &typ_idx in &other_indices {
        Self::intersection(target_idx, typ_idx, sp);
      }
      return;
    }

    // `target` is already a intersection (add to it)
    if matches!(sp.types.get(target_idx), Type::Intersection(..)) {
      let target_indices = match sp.types.get(target_idx) {
        Type::Intersection(indices) => indices.clone(),
        _ => unreachable!(),
      };

      let mut found_type = false;
      for t in target_indices.iter() {
        if sp.types.get(*t) == other {
          found_type = true;
          break;
        }
      }

      if !found_type {
        match sp.types.get_mut(target_idx) {
          Type::Intersection(indices) => indices.push(other_idx),
          _ => unreachable!(),
        }
      }
      return;
    }

    // Neither is a intersection, so make one!
    if sp.types.get(target_idx) != sp.types.get(other_idx) {
      let old_type = sp.types.get(target_idx).clone();
      let old_type_idx = sp.types.add(old_type);

      let mut indices = SmallVec::with_element(old_type_idx);
      indices.push(other_idx);

      let new_intersection = Type::Intersection(indices);
      *sp.types.get_mut(target_idx) = new_intersection;
    }
  }

  // Makes an intersection from a vector of types, removing duplicates before returning
  pub fn new_checked_intersection(
    types: SmallVec<TypeIndex>,
    sp: &mut SourceProperties,
  ) -> TypeIndex {
    let mut out = SmallVec::new();
    for t in types {
      let mut found = false;
      for existing in &out {
        if sp.types.get(t) == sp.types.get(*existing) {
          found = true;
          break;
        }
      }
      if !found {
        out.push(t);
      }
    }
    return sp.types.add(Type::Intersection(out));
  }

  /// Removes a type from a union or intersection, if it exists
  pub fn remove(target: TypeIndex, typ: TypeIndex, sp: &mut SourceProperties) {
    let types = if let Type::Union(types) | Type::Intersection(types) = sp.types.get(target) {
      types
    } else {
      return;
    };

    let mut new_arr = SmallVec::with_capacity(types.len() - 1);
    for e in types.iter() {
      if *e != typ {
        new_arr.push(e.clone());
      }
    }

    if let Type::Union(types) | Type::Intersection(types) = sp.types.get_mut(target) {
      *types = new_arr;
    } else {
      unreachable!();
    }
  }

  pub fn get_single_name(&self, sp: &SourceProperties) -> String {
    match &self {
      Type::Any => "any".to_string(),

      Type::Number => "number".to_string(),

      Type::String => "string".to_string(),
      Type::StringLiteral(string) => string.clone(),

      Type::Boolean => "boolean".to_string(),
      Type::BooleanLiteral(boolean) => {
        if *boolean {
          "true".to_string()
        } else {
          "false".to_string()
        }
      }

      Type::Unknown => "unknown".to_string(),
      Type::Void => "void".to_string(),

      Type::Custom(name) => name.to_string(),

      Type::WithArgs(typ, _) => sp.types.get(*typ).get_single_name(sp),

      Type::Array(typ) => sp.types.get(*typ).get_single_name(sp) + "[]",

      _ => {
        panic!("`get_single_name()` not implemented for {:?}", self)
      }
    }
  }

  pub fn inner_count(&self) -> usize {
    match self {
      Type::Union(inner) => inner.len(),
      Type::Intersection(inner) => inner.len(),
      Type::Object { parts, .. } => parts.len(),
      _ => todo!("type.inner_count not implemented for `{:?}`", self),
    }
  }

  // pub fn index(&self, index_type: TypeIndex) -> TypeIndex {
  //   Type::Unknown
  // }

  // // Indexes into an Array type using a usize index
  // pub fn index_usize(&self, index: usize) -> TypeIndex {
  //   match self {
  //     Type::Array(typ) => (**typ).clone(),
  //     _ => Type::Unknown,
  //   }
  // }

  // // Indexes into an Array type, returning the type of elements in the range [index, ...]
  // pub fn index_spread(&self, index: usize) -> TypeIndex {
  //   match self {
  //     Type::Array(typ) => self.clone(),
  //     _ => Type::Unknown,
  //   }
  // }
}

impl PartialEq for Type {
  fn eq(&self, other: &Self) -> bool {
    use Type::*;
    match (self, other) {
      (Any, Any) => true,
      (Number, Number) => true,
      (NumberLiteral(a), NumberLiteral(b)) => a == b,
      (String, String) => true,
      (StringLiteral(a), StringLiteral(b)) => a == b,
      (Boolean, Boolean) => true,
      (BooleanLiteral(a), BooleanLiteral(b)) => a == b,
      (RegExp, RegExp) => true,
      (Unknown, Unknown) => true,
      (Void, Void) => true,
      (Union(a), Union(b)) => a == b,
      (Intersection(a), Intersection(b)) => a == b,
      (Custom(a), Custom(b)) => a == b,
      (WithArgs(a1, a2), WithArgs(b1, b2)) => a1 == b1 && a2 == b2,
      (Tuple { inner_types: a }, Tuple { inner_types: b }) => a == b,
      (Array(a), Array(b)) => a == b,
      (
        Mapped {
          key_name: akn,
          key_type: akt,
          value_type: avt,
        },
        Mapped {
          key_name: bkn,
          key_type: bkt,
          value_type: bvt,
        },
      ) => akn == bkn && akt == bkt && avt == bvt,
      (Guard(an, at), Guard(bn, bt)) => an == bn && at == bt,
      (
        ColonDeclaration {
          spread: aspr,
          name: an,
          typ: at,
          conditional: ac,
        },
        ColonDeclaration {
          spread: bspr,
          name: bn,
          typ: bt,
          conditional: bc,
        },
      ) => aspr == bspr && an == bn && at == bt && ac == bc,
      (SpreadParameter { name: an }, SpreadParameter { name: bn }) => an == bn,
      (
        Index {
          callee: ac,
          property: ap,
        },
        Index {
          callee: bc,
          property: bp,
        },
      ) => ac == bc && ap == bp,
      (
        DirectAccess {
          callee: ac,
          property: ap,
        },
        DirectAccess {
          callee: bc,
          property: bp,
        },
      ) => ac == bc && ap == bp,
      (TypeOf(a), TypeOf(b)) => a == b,
      (KeyOf(a), KeyOf(b)) => a == b,
      (
        Conditional {
          cnd_left: al,
          cnd_right: ar,
          if_true: at,
          if_false: af,
        },
        Conditional {
          cnd_left: bl,
          cnd_right: br,
          if_true: bt,
          if_false: bf,
        },
      ) => al == bl && ar == br && at == bt && af == bf,
      (Extends(a1, a2), Extends(b1, b2)) => a1 == b1 && a2 == b2,
      (Infer(a), Infer(b)) => a == b,
      (Readonly(a), Readonly(b)) => a == b,
      // _ if std::mem::discriminant(self) == std::mem::discriminant(other) => {
      //   todo!("PartialEq not implemented for this Type variant: {:?}", self)
      // }
      // Otherwise, not equal
      _ => false,
    }
  }
}
impl PartialEq<&Type> for Type {
  fn eq(&self, other: &&Type) -> bool {
    *self == **other
  }
}

impl PartialEq<Type> for &Type {
  fn eq(&self, other: &Type) -> bool {
    **self == *other
  }
}
impl Eq for Type {}

/// Gets a type, regardless of whether it starts with `:` or not.
/// If it *does* start with a `:`, that gets consumed and the type is returned.
pub fn get_type(sp: &mut SourceProperties) -> Result<TypeIndex, CompilerError> {
  if sp.tokens.peek_str() == ":" {
    sp.tokens.skip_unchecked();
  }
  get_expression(0, sp)
}

/// Gets a type only if the next token is `:`.
/// Otherwise, returns None
pub fn try_get_type(sp: &mut SourceProperties) -> Result<Option<TypeIndex>, CompilerError> {
  if sp.tokens.peek_str() != ":" {
    Ok(None)
  } else {
    sp.tokens.skip_unchecked(); // Skip ":"
    Ok(Some(get_expression(0, sp)?))
  }
}

fn parse_infix(
  left: TypeIndex,
  precedence: u8,
  sp: &mut SourceProperties,
) -> Result<TypeIndex, CompilerError> {
  let conditional = sp.tokens.try_skip_and_ignore_whitespace("?");
  let infix_opr = sp.tokens.consume();
  match sp.str_src(infix_opr.value) {
    "|" => {
      Type::union(left, get_expression(precedence, sp)?, sp);
      Ok(left)
    }
    "&" => {
      Type::intersection(left, get_expression(precedence, sp)?, sp);
      Ok(left)
    }
    "is" => {
      // Type guard
      sp.tokens.skip_unchecked();
      sp.tokens.ignore_whitespace();
      let right = get_expression(precedence, sp)?;
      Ok(sp.types.add(Type::Guard(
        // The left side of a type guard is a variable from the scope!
        sp.types.get(left).get_single_name(sp),
        right,
      )))
    }
    ":" => {
      sp.tokens.skip_unchecked();
      sp.tokens.ignore_whitespace();
      let typ = get_expression(precedence, sp)?;
      Ok(sp.types.add(Type::ColonDeclaration {
        spread: false,
        name: sp.types.get(left).get_single_name(sp),
        typ,
        conditional,
      }))
    }
    "[" => {
      // Type indexing
      let mut arguments: SmallVec<TypeIndex> = SmallVec::new();
      sp.tokens.ignore_whitespace();
      if sp.tokens.peek_str() != "]" {
        loop {
          sp.tokens.ignore_whitespace();
          arguments.push(get_expression(0, sp)?);
          if sp.tokens.peek_str() != "," {
            break;
          }
          sp.tokens.skip_unchecked(); // Skip comma
        }
      }
      sp.tokens.skip("]")?;

      if arguments.is_empty() {
        // Normal array notation
        Ok(sp.types.add(Type::Array(left)))
      } else if arguments.len() == 1 {
        Ok(sp.types.add(Type::Index {
          callee: left,
          property: arguments.pop().unwrap(),
        }))
      } else {
        Err(CompilerError::new(
          infix_opr.value,
          format!(
            "Type indices need exactly one argument, found {}",
            arguments.len()
          ),
        ))
      }
    }
    "." => {
      // Direct type access
      sp.tokens.ignore_whitespace();
      let token = sp.tokens.consume_type(TokenType::Identifier)?.value;
      let property = sp.types.add(Type::Custom(sp.str_src(token).to_owned()));
      Ok(sp.types.add(Type::DirectAccess {
        callee: left,
        property,
      }))
    }
    "<" => {
      let generics = get_generics(sp)?;
      Ok(sp.types.add(Type::WithArgs(left, generics)))
    }
    "extends" => {
      // This could be a normal `extends` (like generics), or a conditional
      let extends_precedence: u8 = get_type_operator_binding_power(ExprType::Infx, "extends")
        .unwrap()
        .0;

      // Get the next type
      let right_type = get_expression(extends_precedence, sp)?;

      sp.tokens.ignore_whitespace();
      if sp.tokens.peek_str() != "?" {
        // Not a conditional! Just a normal `extends`
        return Ok(sp.types.add(Type::Extends(left, right_type)));
      }
      sp.tokens.skip_unchecked(); // Skip "?"

      let if_true = get_expression(extends_precedence, sp)?;

      // Skip ":"
      sp.tokens.ignore_whitespace();
      sp.tokens.skip(":")?;

      let if_false = get_expression(extends_precedence, sp)?;

      Ok(sp.types.add(Type::Conditional {
        cnd_left: left,
        cnd_right: right_type,
        if_true,
        if_false,
      }))
    }
    other => Err(CompilerError::new(
      infix_opr.value,
      format!("Unexpected infix in type {:?}", other),
    )),
  }
}

fn parse_name(sp: &mut SourceProperties) -> TypeIndex {
  let token = sp.tokens.consume().value;
  match sp.str_src(token) {
    "any" => sp.types.add(Type::Any),
    "number" => sp.types.add(Type::Number),
    "string" => sp.types.add(Type::String),
    "boolean" => sp.types.add(Type::Boolean),
    "true" => sp.types.add(Type::BooleanLiteral(true)),
    "false" => sp.types.add(Type::BooleanLiteral(false)),
    "void" | "undefined" => sp.types.add(Type::Void),
    other => sp.types.add(Type::Custom(other.to_string())),
  }
}

fn types_into_params(
  types: SmallVec<TypeIndex>,
  sp: &mut SourceProperties,
) -> Result<SmallVec<TypeFunctionArgument>, CompilerError> {
  let mut params = SmallVec::with_capacity(types.len());
  for p in types.iter() {
    params.push(match sp.types.get(*p) {
      Type::ColonDeclaration {
        spread,
        name,
        typ,
        conditional,
      } => TypeFunctionArgument {
        spread: *spread,
        name: name.clone(),
        conditional: *conditional,
        typ: Some(*typ),
      },
      Type::SpreadParameter { name } => TypeFunctionArgument {
        spread: true,
        name: name.clone(),
        conditional: false,
        typ: None,
      },
      Type::Custom(name) => TypeFunctionArgument {
        spread: false,
        name: name.clone(),
        conditional: false,
        typ: None,
      },
      other => {
        return Err(CompilerError::new(
          SrcMapping::empty(), // TODO: replace with real token...
          format!("Expected type argument, found type {:?}", other),
        ));
      }
    });
  }
  Ok(params)
}

fn parse_prefix(precedence: u8, sp: &mut SourceProperties) -> Result<TypeIndex, CompilerError> {
  let prefix_opr = sp.tokens.consume();
  match sp.str_src(prefix_opr.value) {
    "{" => {
      // Curly brace types (Objects or Mapped types)
      parse_curly_braces(sp)
    }
    "[" => {
      // Tuple
      let mut inner_types = SmallVec::new();
      sp.tokens.ignore_commas();
      loop {
        let has_spread = sp.tokens.peek_str() == "...";
        if has_spread {
          sp.tokens.skip_unchecked();
        }
        inner_types.push((get_expression(0, sp)?, has_spread));

        // End on brackets
        if sp.tokens.peek_str() == "]" {
          break;
        }
        sp.tokens.ignore_commas();
      }
      sp.tokens.skip("]")?;
      Ok(sp.types.add(Type::Tuple { inner_types }))
    }
    "(" => {
      // Parenthesized type!
      // This could be an arrow function, too

      let mut paren_contents = SmallVec::with_capacity(4);
      sp.tokens.ignore_commas();
      loop {
        sp.tokens.ignore_whitespace();
        if sp.tokens.peek_str() == ")" {
          break;
        }

        paren_contents.push(get_type(sp)?);
        if !sp.tokens.ignore_commas() {
          break;
        }
      }
      sp.tokens.skip_unchecked(); // Skip ")"
      sp.tokens.ignore_whitespace();

      if sp.tokens.peek_str() != "=>" && sp.tokens.peek_str() != ":" {
        // Normal parenthesized type
        if paren_contents.is_empty() {
          // Empty parenthesis?
          Err(CompilerError::expected(sp.tokens.consume().value, "=>"))
        } else if paren_contents.len() == 1 {
          // Remove the type from inside!
          Ok(paren_contents.pop().unwrap())
        } else {
          let func = Type::Function {
            generics: SmallVec::new(),
            params: types_into_params(paren_contents, sp)?,
            return_type: sp.types.add(Type::Unknown),
            is_constructor: false,
          };
          Ok(sp.types.add(func))
        }
      } else {
        // Function
        let params = types_into_params(paren_contents, sp)?;
        sp.tokens.skip_unchecked();
        let return_type = get_expression(precedence, sp)?;
        let func = Type::Function {
          generics: SmallVec::new(),
          params,
          return_type,
          is_constructor: false,
        };
        Ok(sp.types.add(func))
      }
    }
    "-" => {
      let next_token = sp.tokens.peek().clone();
      let next_typ = get_expression(precedence, sp)?;
      match sp.types.get(next_typ) {
        Type::NumberLiteral(lit) => Ok(sp.types.add(Type::NumberLiteral(-*lit))),
        other => {
          return Err(CompilerError::new(
            next_token.value,
            format!("Expected number after \"-\", found {:?}", other),
          ));
        }
      }
    }
    "new" => {
      // Makes the proceeding function a constructor
      let next_token = sp.tokens.peek().clone();
      let next_type = get_expression(precedence, sp)?;
      match sp.types.get_mut(next_type) {
        Type::Function { is_constructor, .. } => {
          *is_constructor = true;
        }
        other => {
          return Err(CompilerError::new(
            next_token.value,
            format!("`new` prefix expected \"Function\", found {:?}", other),
          ))
        }
      }
      Ok(next_type)
    }
    "..." => {
      // Spread for arguments
      let param = get_expression(0, sp)?;
      match sp.types.get(param) {
        Type::ColonDeclaration { .. } => {
          if let Type::ColonDeclaration { spread, .. } = sp.types.get_mut(param) {
            *spread = true;
          }
          Ok(param)
        }
        Type::Custom(name) => Ok(sp.types.add(Type::SpreadParameter { name: name.clone() })),
        _ => {
          Err(CompilerError::new(
            SrcMapping::empty(), // TODO: replace with real token
            format!("Expected parameter after spread, found {:?}", param),
          ))
        }
      }
    }
    "|" | "&" => {
      // Prefix symbols used as syntactic sugar are ignored
      get_expression(precedence, sp)
    }
    "<" => {
      // Start of function with generics
      let mut generics = get_generics(sp)?;
      let next_token = sp.tokens.peek().clone();
      let next_fn = get_expression(precedence, sp)?;
      match sp.types.get_mut(next_fn) {
        Type::Function {
          generics: inner_generics,
          ..
        } => {
          inner_generics.append(&mut generics);
        }
        other => {
          return Err(CompilerError::new(
            next_token.value,
            format!("Expected Function type, found {:?}", other),
          ))
        }
      }
      Ok(next_fn)
    }
    "asserts" | "unique" | "abstract" => {
      // These types are ignored
      get_expression(precedence, sp)
    }
    "typeof" => {
      let to = Type::TypeOf(get_expression(precedence, sp)?);
      Ok(sp.types.add(to))
    }
    "keyof" => {
      let to = Type::KeyOf(get_expression(precedence, sp)?);
      Ok(sp.types.add(to))
    }
    "infer" => {
      sp.tokens.ignore_whitespace();
      let token = sp.tokens.consume().value;
      Ok(sp.types.add(Type::Infer(sp.str_src(token).to_owned())))
    }
    "readonly" => {
      let ro = Type::Readonly(get_expression(precedence, sp)?);
      Ok(sp.types.add(ro))
    }
    other => Err(CompilerError::new(
      prefix_opr.value,
      format!("Type prefix operator not found: {:?}", other),
    )),
  }
}

fn parse_curly_braces(sp: &mut SourceProperties) -> Result<TypeIndex, CompilerError> {
  let mut obj_parts = SmallVec::new();
  let mut kv_maps = SmallVec::new();
  let mut mapped_type: Option<TypeIndex> = None;

  sp.tokens.ignore_commas();
  loop {
    sp.tokens.ignore_whitespace();
    if sp.tokens.peek_str() == "}" {
      sp.tokens.skip_unchecked();
      break;
    }

    let property = if sp.tokens.peek_str() == "[" {
      let osbr = parse_object_square_bracket(sp)?;
      match osbr {
        ObjectSquareBracketReturn::KVMap(kv_map) => {
          kv_maps.push(kv_map);
          continue;
        }
        ObjectSquareBracketReturn::ComputedProp(value) => {
          ComputableDeclarationName::Computed(value)
        }
        ObjectSquareBracketReturn::MappedType(typ) => {
          if mapped_type.is_some() {
            return Err(CompilerError::new(
              sp.tokens.consume().value,
              "Can't have multiple mapped types in one object.".to_string(),
            ));
          }
          mapped_type = Some(typ.clone());
          continue;
        }
      }
    } else {
      ComputableDeclarationName::Named(sp.tokens.consume().value)
    };

    // Get property type
    sp.tokens.ignore_whitespace();
    let property_type = if sp.tokens.peek_str() == ":" {
      sp.tokens.skip_unchecked(); // Skip ":"
      get_expression(*crate::operations::COMMA_PRECEDENCE, sp)?
    } else {
      sp.types.add(Type::Unknown)
    };

    // Push
    obj_parts.push(DeclarationTyped::from_parts(property, property_type));

    // Ignore commas / exit
    sp.tokens.ignore_commas();
  }

  if mapped_type.is_some() {
    return Ok(mapped_type.unwrap());
  }

  Ok(sp.types.add(Type::Object {
    key_value: kv_maps,
    parts: obj_parts,
  }))
}

pub fn parse_object_square_bracket(
  sp: &mut SourceProperties,
) -> Result<ObjectSquareBracketReturn, CompilerError> {
  sp.tokens.skip("[")?;
  sp.tokens.ignore_whitespace();

  if sp.tokens.peek().typ != TokenType::Identifier {
    // Anything that isn't an identifier is a computed property
    let computed = parser::get_expression(0, sp)?;
    sp.tokens.skip("]")?;
    return Ok(ObjectSquareBracketReturn::ComputedProp(computed));
  }

  let checkpoint = sp.tokens.get_checkpoint();

  // Although this is usually "key", it can be any identifier!
  let key_token = sp.tokens.consume();
  sp.tokens.ignore_whitespace();

  if sp.tokens.peek_str() == "in" {
    sp.tokens.ignore_checkpoint(checkpoint);
    return get_kvc_complex_key(key_token, sp);
  }

  sp.tokens.ignore_whitespace();
  if sp.tokens.peek_str() != ":" {
    // It's computed!
    sp.tokens.restore_checkpoint(checkpoint);
    let computed = parser::get_expression(0, sp)?;
    sp.tokens.skip("]")?;
    return Ok(ObjectSquareBracketReturn::ComputedProp(computed));
  } else {
    sp.tokens.ignore_checkpoint(checkpoint);
  }
  sp.tokens.skip(":")?;

  let key_type = get_expression(0, sp)?;

  sp.tokens.ignore_whitespace();
  sp.tokens.skip("]")?;

  sp.tokens.ignore_whitespace();
  let is_optional = sp.tokens.try_skip_and_ignore_whitespace("?");
  sp.tokens.skip(":")?;

  let value_type = get_expression(0, sp)?;
  if is_optional {
    // Make value type also include `undefined`
    Type::union(value_type, sp.types.add(Type::Void), sp);
  }

  Ok(ObjectSquareBracketReturn::KVMap(KeyValueMap {
    key: key_type,
    value: value_type,
  }))
}

fn get_kvc_complex_key(
  key_token: Token,
  sp: &mut SourceProperties,
) -> Result<ObjectSquareBracketReturn, CompilerError> {
  let key_name = sp.str_src(key_token.value).to_owned();
  sp.tokens.skip("in")?;
  sp.tokens.ignore_whitespace();
  let key_type = get_expression(0, sp)?;
  sp.tokens.ignore_whitespace();
  sp.tokens.skip("]")?;
  sp.tokens.ignore_whitespace();
  let is_optional = sp.tokens.try_skip_and_ignore_whitespace("?");
  sp.tokens.skip(":")?;
  let value_type = get_expression(0, sp)?;
  if is_optional {
    // Make value type also include `undefined`
    Type::union(value_type, sp.types.add(Type::Void), sp);
  }

  let mapped_type = Type::Mapped {
    key_name,
    key_type: key_type,
    value_type: value_type,
  };
  return Ok(ObjectSquareBracketReturn::MappedType(
    sp.types.add(mapped_type),
  ));
}

fn get_expression(precedence: u8, sp: &mut SourceProperties) -> Result<TypeIndex, CompilerError> {
  sp.tokens.ignore_whitespace();

  let mut left = {
    let next = sp.tokens.peek();
    // println!("Handling prefix {:?}", next);

    if [")", "]", "}", ",", ";"].contains(&sp.str_src(next.value)) {
      // End it here!
      return Ok(sp.types.add(Type::Unknown));
    }

    // let next = sp.tokens.peek();
    match &next.typ {
      TokenType::Number => {
        let token = sp.tokens.consume().value;
        get_numeric_literal_type(sp.str_src(token), sp)
      }
      TokenType::String => {
        let token = sp.tokens.consume().value;
        sp.types
          .add(Type::StringLiteral(sp.str_src(token).to_owned()))
      }
      TokenType::Identifier | TokenType::Symbol => {
        let binding_power =
          get_type_operator_binding_power(ExprType::Prefx, sp.str_src(next.value));
        if let Some(binding_power) = binding_power {
          // Prefix operators
          parse_prefix(binding_power.1, sp)?
        } else if next.typ == TokenType::Identifier {
          // Could be a name...
          parse_name(sp)
        } else {
          return Err(CompilerError::new(
            sp.tokens.consume().value,
            format!("Prefix operator not found when fetching type: {:?}", next),
          ));
        }
      }
      other => {
        return Err(CompilerError::new(
          sp.tokens.consume().value,
          format!("Unexpected token when parsing type: {:?}", other),
        ));
      }
    }
  };

  loop {
    // Get the next token
    sp.tokens.ignore_whitespace();
    let next = sp.tokens.peek();
    if next.typ == TokenType::EndOfFile {
      break;
    }

    // println!(
    //   "Handling infix/postfix (w/ min precedence {}) in type: {:?}",
    //   precedence, next
    // );

    // Handle infix
    let binding_power = if let Some(binding_power) =
      get_type_operator_binding_power(ExprType::Infx, sp.str_src(next.value))
    {
      if binding_power.0 < precedence {
        break;
      }
      binding_power
    } else {
      // No infix for this symbol!
      break;
    };

    left = parse_infix(left, binding_power.1, sp)?;
  }

  Ok(left)
}

/// Gets types separated by commas
pub fn get_comma_separated_types_until(
  until_str: &[&str],
  sp: &mut SourceProperties,
) -> Result<SmallVec<TypeIndex>, CompilerError> {
  let mut types = SmallVec::new();
  sp.tokens.ignore_commas();
  loop {
    if until_str.contains(&sp.tokens.peek_str()) {
      break;
    }
    types.push(get_type(sp)?);
    if sp.tokens.peek_str() == "=" {
      // Type defaults are ignored! They're an artifact of TypeScript's
      // older type inference, which couldn't infer a lot of the more
      // complex types.
      sp.tokens.skip_unchecked();
      get_type(sp)?;
    }
    if !sp.tokens.ignore_commas() {
      break;
    }
  }
  Ok(types)
}

/// Gets generics if available, otherwise returns an empty vec
pub fn get_optional_generics(
  sp: &mut SourceProperties,
) -> Result<SmallVec<TypeIndex>, CompilerError> {
  // Get generics
  if sp.tokens.peek_str() != "<" {
    return Ok(SmallVec::new());
  }
  sp.tokens.skip_unchecked(); // Skip "<"
  get_generics(sp)
}

/// Gets generics until it finds a ">", consuming it.
/// Used after the first "<", as it will not consume it!
pub fn get_generics(sp: &mut SourceProperties) -> Result<SmallVec<TypeIndex>, CompilerError> {
  let generics = get_comma_separated_types_until(&[">", ")", ";"], sp)?;
  if !sp.tokens.peek_str().starts_with(">") {
    return Err(CompilerError::expected(sp.tokens.consume().value, ">"));
  }
  if sp.tokens.peek_str().len() > 1 {
    let _ = sp.tokens.consume_single_character();
  } else {
    sp.tokens.skip_unchecked(); // Skip ">"
  }
  Ok(generics)
}

pub fn get_numeric_literal_type(num_str: &str, sp: &mut SourceProperties) -> TypeIndex {
  let mut is_bigint = false;
  let trimmed_string = if num_str.ends_with("n") {
    is_bigint = true;
    num_str[0..num_str.len() - 2].to_owned()
  } else {
    num_str.to_owned()
  };

  let t = if let Ok(value) = trimmed_string.parse::<f64>() {
    Type::NumberLiteral(CustomDouble { value })
  } else {
    Type::Number
  };

  sp.types.add(if is_bigint {
    match t {
      Type::Number => Type::BigInt,
      Type::NumberLiteral(n) => Type::BigIntLiteral(n),
      _ => unreachable!(),
    }
  } else {
    t
  })
}
