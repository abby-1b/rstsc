
use crate::{
  ast_common::*, declaration::Declaration, error_type::CompilerError, small_vec::SmallVec, spread::Spread, tokenizer::Token, types::{KeyValueMap, Type}
};

#[derive(Debug, Clone, PartialEq, Hash)]
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

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct FunctionDefinition {
  pub modifiers: ModifierList,
  pub name: Option<String>,

  pub generics: SmallVec<Type>,
  pub params: SmallVec<Declaration>,
  pub spread: Spread,
  pub return_type: Option<Type>,
  pub body: Option<ASTNode>
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct ArrowFunctionDefinition {
  pub params: SmallVec<Declaration>,
  pub spread: Spread,
  pub return_type: Type,
  pub body: ASTNode
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct ClassDefinition {
  pub modifiers: ModifierList,
  pub name: Option<String>,
  pub generics: SmallVec<Type>,
  pub extends: Option<Type>,
  pub implements: SmallVec<Type>,
  pub kv_maps: SmallVec<KeyValueMap>,

  /// Named declarations
  pub declarations: SmallVec<(ModifierList, Declaration)>,

  /// Functions
  pub methods: SmallVec<FunctionDefinition>
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct InterfaceDeclaration {
  pub name: String,
  pub generics: SmallVec<Type>,
  pub extends: SmallVec<Type>,
  pub equals_type: Type
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct EnumDeclaration {
  pub name: String,
  pub members: SmallVec<(String, ASTNode)>,
  pub is_const: bool
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum ASTNode {
  /// A block of code
  Block { nodes: SmallVec<ASTNode> },

  /// A `var`, `let` or `const` variable definition
  VariableDeclaration {
    modifiers: ModifierList,
    def_type: VariableDefType,
    defs: SmallVec<Declaration>
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
  ArrowFunctionDefinition { inner: Box<ArrowFunctionDefinition> },

  ClassDefinition { inner: Box<ClassDefinition> },

  // Used for expressions

  Parenthesis { nodes: SmallVec<ASTNode> },
  Array { nodes: SmallVec<ASTNode> },
  Dict { properties: SmallVec<ObjectProperty> },

  // Expression parsing...

  ExprIdentifier { name: String },
  ExprNumLiteral { number: String },
  ExprStrLiteral { string: String },
  ExprBoolLiteral { value: bool },

  ExprFunctionCall {
    callee: Box<ASTNode>,
    generics: SmallVec<Type>,
    arguments: SmallVec<ASTNode>,
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
    cast_type: Box<Type>
  },

  /// Casting something C-style (eg. `<any>obj`)
  ExprTypeAssertion {
    cast_type: Box<Type>,
    value: Box<ASTNode>,
  },

  EnumDeclaration { inner: Box<EnumDeclaration> },
  InterfaceDeclaration { inner: Box<InterfaceDeclaration> },

  /// Used in situations like `[ 1, 2, ]` where there's an empty expression
  Empty
}

impl ASTNode {
  /// Gets the name of this enum, without the associated data
  pub fn name(&self) -> String {
    match self {
      ASTNode::Block { .. } => "Block",
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
      ASTNode::EnumDeclaration { .. } => "EnumDeclaration",
      ASTNode::InterfaceDeclaration { .. } => "InterfaceDeclaration",
      ASTNode::Empty { .. } => "Empty",
    }.to_string()
  }

  /// Checks if a node is a literal
  pub fn is_literal(&self) -> bool {
    matches!(
      self,
      ASTNode::ExprNumLiteral { .. } | ASTNode::ExprStrLiteral { .. } | ASTNode::ExprBoolLiteral { .. }
    )
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
          token: self.as_token()
        });
      }
    }
    Ok(())
  }

  pub fn as_token<'a>(&self) -> Token<'a> {
    // TODO: convert this node into a single token
    Token::from("")
  }
}
