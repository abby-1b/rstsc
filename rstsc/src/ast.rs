
use core::fmt::Debug;

use crate::{error_type::CompilerError, small_vec::SmallVec, tokenizer::EOF_TOKEN, types::Type};

#[derive(Debug, Clone, PartialEq)]
pub enum VariableDefType {
  Var,
  Let,
  Const
}
impl VariableDefType {
  pub fn emit(&self) -> String {
    match self {
      VariableDefType::Var => "var ",
      VariableDefType::Let => "let ",
      VariableDefType::Const => "const ",
    }.to_string()
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ObjectProperty {
  Property {
    computed: bool,
    key: ASTNode,
    value: ASTNode
  },
  Spread {
    argument: ASTNode
  }
}

#[derive(Default, Clone, PartialEq)]
/// A list of modifier flags
pub struct ModifierList {
  /// A list of bit flags (lowest to highest) detailing:
  ///  - export
  ///  - async
  ///  - static
  ///  - public
  ///  - private
  ///  - protected
  ///  - readonly
  pub flags: u8,
}

pub const ACCESSIBILITY_MODIFIERS: &[&str] = &[
  "public", "private", "protected",
];

pub const MODIFIERS: &[&str] = &[
  "export",
  "async",
  "static",
  "public", "private", "protected",
  "readonly",
  "abstract"
];
pub const MODIFIER_IS_JS: &[bool] = &[
  true,
  true,
  true,
  false, false, false,
  false,
  false
];

#[derive(Clone)]
/// A single modifier
pub enum Modifier {
  Export = 1,
  Async = 2,
  Static = 4,
  Public = 8,
  Private = 16,
  Protected = 32,
  Readonly = 64,
  Abstract = 128,
}
impl Into<u8> for Modifier {
  fn into(self) -> u8 {
    match self {
      Modifier::Export => 1,
      Modifier::Async => 2,
      Modifier::Static => 4,
      Modifier::Public => 8,
      Modifier::Private => 16,
      Modifier::Protected => 32,
      Modifier::Readonly => 64,
      Modifier::Abstract => 128,
    }
  }
}

impl ModifierList {
  /// Sets the modifier in the list
  pub fn set(&mut self, modifier: Modifier) {
    self.flags |= modifier as u8;
  }

  /// Checks if the modifier exists within the list
  pub fn has(&self, modifier: Modifier) -> bool {
    self.flags & modifier as u8 != 0
  }

  pub fn emit(&self, js_only: bool) -> String {
    let mut out = String::new();
    for idx in 0..6 {
      let flag = 1 << idx;
      if self.flags & flag == 0 || (js_only && !MODIFIER_IS_JS[idx]) {
        continue;
      }
      out += MODIFIERS[idx];
      out += " ";
    }
    out
  }
}

impl Debug for ModifierList {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Num: {}\n", self.flags)?;
    f.debug_list()
      .entries(
        (0..6).filter_map(|idx| {
          let flag = 1 << idx;
          if self.flags & flag == 0 {
            return None;
          }
          Some(MODIFIERS[idx])
        })
      )
      .finish()
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NamedDeclaration {
  pub name: String,
  pub typ: Option<Type>,
  pub value: Option<ASTNode>,

  /// True when this declaration is conditional
  /// (eg. `someVar?: string`)
  pub conditional: bool,

  /// True when this declaration starts with a spread
  /// (eg. `(...args)`)
  pub spread: bool
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
  pub modifiers: ModifierList,
  pub name: Option<String>,

  pub generics: Vec<Type>,
  pub params: Vec<NamedDeclaration>,
  pub return_type: Option<Type>,
  pub body: Option<Box<ASTNode>>
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassDefinition {
  pub modifiers: ModifierList,
  pub name: Option<String>,
  pub generics: Vec<Type>,
  pub extends: Option<Type>,
  pub implements: Vec<Type>,

  /// Named declarations
  pub declarations: Vec<(ModifierList, NamedDeclaration)>,

  /// Functions
  pub methods: Vec<FunctionDefinition>
}

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceDeclaration {
  pub name: String,
  pub generics: Vec<Type>,
  pub extends: Vec<Type>,
  pub equals_type: Type
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTNode {
  /// A block of code
  Block { nodes: Vec<ASTNode> },

  /// A declaration that contains a name and type.
  /// Can also contain an optional value
  Declaration {
    on: Box<ASTNode>,
    typ: Type,

    // Whether or not this declaration has the `?` sign before its type
    conditional: bool
  },

  /// A `var`, `let` or `const` variable definition
  VariableDeclaration {
    modifiers: ModifierList,
    def_type: VariableDefType,
    defs: Vec<NamedDeclaration>
  },

  StatementIf {
    condition: Box<ASTNode>,
    body: Box<ASTNode>,
    alternate: Option<Box<ASTNode>>
  },
  StatementWhile {
    condition: Box<ASTNode>,
    body: Box<ASTNode>
  },
  StatementFor {
    init: Box<ASTNode>,
    condition: Box<ASTNode>,
    update: Box<ASTNode>,
    body: Box<ASTNode>
  },

  StatementReturn { value: Option<Box<ASTNode>> },
  StatementBreak { value: Option<Box<ASTNode>> },
  StatementContinue { value: Option<Box<ASTNode>> },
  StatementThrow { value: Option<Box<ASTNode>> },

  FunctionDefinition { inner: Box<FunctionDefinition> },

  ArrowFunctionDefinition {
    params: Vec<NamedDeclaration>,
    return_type: Option<Type>,
    body: Box<ASTNode>
  },

  ClassDefinition { inner: Box<ClassDefinition> },

  // Used for expressions

  Parenthesis { nodes: Vec<ASTNode> },
  Array { nodes: Vec<ASTNode> },
  Dict { properties: Vec<ObjectProperty> },

  // Expression parsing...

  ExprIdentifier { name: String },
  ExprNumLiteral { number: String },
  ExprStrLiteral { string: String },
  ExprBoolLiteral { value: bool },

  ExprFunctionCall {
    callee: Box<ASTNode>,
    generics: Vec<Type>,
    arguments: Vec<ASTNode>,
  },
  ExprIndexing { callee: Box<ASTNode>, property: Box<ASTNode> },

  ExprTernary {
    condition: Box<ASTNode>,
    if_true: Box<ASTNode>,
    if_false: Box<ASTNode>
  },

  PrefixOpr { opr: String, expr: Box<ASTNode> },
  InfixOpr { left: Box<ASTNode>, opr: String, right: Box<ASTNode> },
  PostfixOpr { expr: Box<ASTNode>, opr: String },
  NonNullAssertion { expr: Box<ASTNode> },

  // Type casting

  /// Casting something with the `as` keyword (eg. `obj as any`)
  ExprAs {
    value: Box<ASTNode>,
    cast_type: Type
  },

  /// Casting something C-style (eg. `<any>obj`)
  ExprTypeAssertion {
    cast_type: Type,
    value: Box<ASTNode>,
  },

  InterfaceDeclaration { inner: Box<InterfaceDeclaration> },

  /// Used in situations like `[ 1, 2, ]` where there's an empty expression
  Empty
}

impl ASTNode {
  /// Gets the name of this enum, without the associated data
  pub fn name(&self) -> String {
    match self {
      ASTNode::Block { .. } => "Block",
      ASTNode::Declaration { .. } => "Declaration",
      ASTNode::VariableDeclaration { .. } => "VariableDeclaration",
      ASTNode::StatementIf { .. } => "StatementIf",
      ASTNode::StatementWhile { .. } => "StatementWhile",
      ASTNode::StatementFor { .. } => "StatementFor",
      ASTNode::StatementReturn { .. } => "StatementReturn",
      ASTNode::StatementBreak { .. } => "StatementBreak",
      ASTNode::StatementContinue { .. } => "StatementContinue",
      ASTNode::StatementThrow { .. } => "StatementThrow",
      ASTNode::FunctionDefinition { .. } => "FunctionDefinition",
      ASTNode::ArrowFunctionDefinition { .. } => "ArrowFunctionDefinition",
      ASTNode::ClassDefinition { .. } => "ClassDefinition",
      ASTNode::Parenthesis { .. } => "Parenthesis",
      ASTNode::Array { .. } => "Array",
      ASTNode::Dict { .. } => "Dict",
      ASTNode::ExprNumLiteral { .. } => "ExprNumLiteral",
      ASTNode::ExprStrLiteral { .. } => "ExprStrLiteral",
      ASTNode::ExprIdentifier { .. } => "ExprIdentifier",
      ASTNode::ExprBoolLiteral { .. } => "ExprBoolLiteral",
      ASTNode::ExprFunctionCall { .. } => "ExprFunctionCall",
      ASTNode::ExprIndexing { .. } => "ExprIndexing",
      ASTNode::ExprTernary { .. } => "ExprTernary",
      ASTNode::PrefixOpr { .. } => "PrefixOpr",
      ASTNode::InfixOpr { .. } => "InfixOpr",
      ASTNode::PostfixOpr { .. } => "PostfixOpr",
      ASTNode::NonNullAssertion { .. } => "NonNullAssertion",
      ASTNode::ExprAs { .. } => "ExprAs",
      ASTNode::ExprTypeAssertion { .. } => "ExprTypeAssertion",
      ASTNode::InterfaceDeclaration { .. } => "InterfaceDeclaration",
      ASTNode::Empty { .. } => "Empty",
    }.to_string()
  }

  /// Checks if a node is a literal
  pub fn is_literal(&self) -> bool {
    match self {
      ASTNode::ExprNumLiteral { .. } => true,
      ASTNode::ExprStrLiteral { .. } => true,
      ASTNode::ExprBoolLiteral { .. } => true,
      _ => false
    }
  }

  pub fn apply_modifiers(&mut self, new_modifiers: ModifierList) -> Result<(), CompilerError<'static>> {
    match self {
      ASTNode::VariableDeclaration { modifiers, .. } => {
        *modifiers = new_modifiers;
      }
      ASTNode::FunctionDefinition { inner } => {
        inner.modifiers = new_modifiers;
      }
      ASTNode::ClassDefinition { inner } => {
        inner.modifiers = new_modifiers;
      }
      _ => {
        return Err(CompilerError {
          message: format!(
            "{} can't have modifiers!",
            self.name()
          ),
          token: EOF_TOKEN.clone()
        });
      }
    }
    Ok(())
  }
}
