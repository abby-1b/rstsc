use crate::{
  ast_common::*, declaration::{DeclarationComputable, DestructurableDeclaration}, error_type::CompilerError, small_vec::SmallVec, rest::Rest, tokenizer::Token, types::{KeyValueMap, Type}
};

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum ObjectProperty {
  Property {
    computed: bool,
    key: ASTNode,
    value: ASTNode
  },
  Rest {
    argument: ASTNode
  },
  Shorthand { key: String }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct FunctionDefinition {
  pub modifiers: ModifierList,

  pub name: Option<String>,

  pub generics: SmallVec<Type>,
  pub params: SmallVec<DestructurableDeclaration>,
  pub rest: Rest,
  pub return_type: Option<Type>,
  pub body: Option<ASTNode>
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct ArrowFunctionDefinition {
  pub is_async: bool,
  pub generics: SmallVec<Type>,
  pub params: SmallVec<DestructurableDeclaration>,
  pub rest: Rest,
  pub return_type: Type,
  pub body: ASTNode
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum GetterSetter {
  None,
  Getter,
  Setter
}

impl GetterSetter {
  pub fn as_str(&self) -> &str {
    match self {
      GetterSetter::None => "",
      GetterSetter::Getter => "get",
      GetterSetter::Setter => "set"
    }
  }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum ClassMember {
  Property(DeclarationComputable, ModifierList),
  Method(FunctionDefinition, GetterSetter),
  StaticBlock(ASTNode),
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct ClassDefinition {
  pub modifiers: ModifierList,
  pub name: Option<String>,
  pub generics: SmallVec<Type>,
  pub extends: Option<Type>,
  pub implements: SmallVec<Type>,
  pub kv_maps: SmallVec<KeyValueMap>,

  pub members: SmallVec<ClassMember>,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct TryCatchFinally {
  pub block_try: ASTNode,
  pub capture_catch: Option<String>,
  pub capture_catch_type: Option<Type>,
  pub block_catch: Option<ASTNode>,
  pub block_finally: Option<ASTNode>,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct IndividualImport {
  pub name: String,
  pub alias: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct ExportSpecifier {
  pub name: String,
  pub alias: Option<String>,
  pub is_type: bool,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct ExportDeclaration {
  pub specifiers: SmallVec<ExportSpecifier>,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum ImportDefinition {
  /// Used in `import Defaults from '...'`
  DefaultAliased { source: String, alias: String },
  /// Used in `import * as Alias from '...'`
  AllAsAlias { source: String, alias: String },
  /// Used in `import { A, B, C } from '...'`
  Individual { source: String, parts: SmallVec<IndividualImport> },
  /// Used in `import '...'`
  SourceOnly { source: String },
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct InterfaceDeclaration {
  pub modifiers: ModifierList,
  pub name: String,
  pub generics: SmallVec<Type>,
  pub extends: SmallVec<Type>,
  pub equals_type: Type
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct EnumDeclaration {
  pub modifiers: ModifierList,
  pub name: String,
  pub members: SmallVec<(String, ASTNode)>,
  pub is_const: bool,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum ASTNode {
  /// A block of code
  Block { nodes: SmallVec<ASTNode> },

  /// A `var`, `let` or `const` variable definition
  VariableDeclaration {
    modifiers: ModifierList,
    def_type: VariableDefType,
    defs: SmallVec<DestructurableDeclaration>
  },

  StatementImport { inner: Box<ImportDefinition> },
  ExpressionImport { value: Box<ASTNode> },

  /// Block export statement (e.g., `export { Real, type Fake }`)
  StatementExport { inner: Box<ExportDeclaration> },

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
  StatementForOf {
    init: Box<ASTNode>,
    expression: Box<ASTNode>,
    body: Box<ASTNode>
  },
  StatementForIn {
    init: Box<ASTNode>,
    expression: Box<ASTNode>,
    body: Box<ASTNode>
  },

  StatementSwitch {
    condition: Box<ASTNode>,
    cases: SmallVec<(ASTNode, SmallVec<ASTNode>)>,
    default: Option<SmallVec<ASTNode>>
  },

  StatementReturn { value: Option<Box<ASTNode>> },
  StatementBreak { value: Option<Box<ASTNode>> },
  StatementContinue { value: Option<Box<ASTNode>> },
  StatementThrow { value: Option<Box<ASTNode>> },

  StatementTryCatchFinally { inner: Box<TryCatchFinally> },

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
  ExprRegexLiteral { pattern: String, flags: String },
  ExprTemplateLiteral { head: String, parts: SmallVec<(ASTNode, String)> },
  ExprBoolLiteral { value: bool },

  ExprFunctionCall {
    callee: Box<ASTNode>,
    generics: SmallVec<Type>,
    arguments: SmallVec<ASTNode>,
  },
  TemplateLiteralTag {
    callee: Box<ASTNode>,
    argument: Box<ASTNode>,
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

  /// `declare namespace X { ... }`
  DeclareNamespace {
    name: String,
    types: Vec<ASTNode>,
  },

  /// Used in situations like `[ 1, 2, ]` where there's an empty expression
  Empty
}

impl ASTNode {
  /// Gets the name of this enum, without the associated data
  pub fn name(&self) -> String {
    match self {
      ASTNode::Block { .. } => "Block",
      ASTNode::VariableDeclaration { .. } => "VariableDeclaration",
      ASTNode::StatementImport { .. } => "StatementImport",
      ASTNode::ExpressionImport { .. } => "ExpressionImport",
      ASTNode::StatementExport { .. } => "StatementExport",
      ASTNode::StatementIf { .. } => "StatementIf",
      ASTNode::StatementWhile { .. } => "StatementWhile",
      ASTNode::StatementFor { .. } => "StatementFor",
      ASTNode::StatementForOf { .. } => "StatementForOf",
      ASTNode::StatementForIn { .. } => "StatementForIn",
      ASTNode::StatementSwitch { .. } => "StatementSwitch",
      ASTNode::StatementReturn { .. } => "StatementReturn",
      ASTNode::StatementBreak { .. } => "StatementBreak",
      ASTNode::StatementContinue { .. } => "StatementContinue",
      ASTNode::StatementThrow { .. } => "StatementThrow",
      ASTNode::StatementTryCatchFinally { .. } => "StatementTryCatchFinally",
      ASTNode::FunctionDefinition { .. } => "FunctionDefinition",
      ASTNode::ArrowFunctionDefinition { .. } => "ArrowFunctionDefinition",
      ASTNode::ClassDefinition { .. } => "ClassDefinition",
      ASTNode::Parenthesis { .. } => "Parenthesis",
      ASTNode::Array { .. } => "Array",
      ASTNode::Dict { .. } => "Dict",
      ASTNode::ExprNumLiteral { .. } => "ExprNumLiteral",
      ASTNode::ExprStrLiteral { .. } => "ExprStrLiteral",
      ASTNode::ExprRegexLiteral { .. } => "ExprRegexLiteral",
      ASTNode::ExprTemplateLiteral { .. } => "ExprTemplateLiteral",
      ASTNode::ExprIdentifier { .. } => "ExprIdentifier",
      ASTNode::ExprBoolLiteral { .. } => "ExprBoolLiteral",
      ASTNode::ExprFunctionCall { .. } => "ExprFunctionCall",
      ASTNode::TemplateLiteralTag { .. } => "TemplateLiteralTag",
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
      ASTNode::DeclareNamespace { .. } => "DeclareNamespace",
      ASTNode::Empty { .. } => "Empty",
    }.to_string()
  }

  /// Checks if a node is a literal
  pub fn is_literal(&self) -> bool {
    matches!(
      self,
      ASTNode::ExprNumLiteral { .. } | ASTNode::ExprStrLiteral { .. } | ASTNode::ExprRegexLiteral { .. } | ASTNode::ExprBoolLiteral { .. }
    )
  }

  pub fn apply_modifiers(&mut self, new_modifiers: ModifierList) -> Result<(), CompilerError> {
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
      ASTNode::EnumDeclaration { inner } => {
        inner.modifiers = new_modifiers;
      }
      ASTNode::InterfaceDeclaration { inner } => {
        inner.modifiers = new_modifiers;
      }
      _ => {
        return Err(CompilerError::new_static(
          format!("{} can't have modifiers!", self.name())
        ));
      }
    }
    Ok(())
  }

  pub fn as_token<'a>(&self) -> Token<'a> {
    // TODO: convert this node into a single token
    Token::from("")
  }
}
