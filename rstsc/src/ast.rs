use crate::{
  ast_common::*, declaration::{DeclarationComputable, DestructurableDeclaration}, error_type::CompilerError, small_vec::SmallVec, rest::Rest, tokenizer::Token, types::{KeyValueMap, Type}
};

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum ObjectProperty {
  Property {
    computed: bool,
    key: ASTIndex,
    value: ASTIndex
  },
  Rest {
    argument: ASTIndex
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
  pub body: Option<ASTIndex>
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct ArrowFunctionDefinition {
  pub is_async: bool,
  pub generics: SmallVec<Type>,
  pub params: SmallVec<DestructurableDeclaration>,
  pub rest: Rest,
  pub return_type: Type,
  pub body: ASTIndex
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
  StaticBlock(ASTIndex),
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
  pub block_try: ASTIndex,
  pub capture_catch: Option<String>,
  pub capture_catch_type: Option<Type>,
  pub block_catch: Option<ASTIndex>,
  pub block_finally: Option<ASTIndex>,
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
  pub members: SmallVec<(String, ASTIndex)>,
  pub is_const: bool,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Switch {
  pub condition: ASTIndex,
  pub cases: SmallVec<(ASTIndex, SmallVec<ASTIndex>)>,
  pub default: Option<SmallVec<ASTIndex>>,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct NamespaceDeclaration {
  name: String,
  types: SmallVec<ASTIndex>,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct ExprTemplateLiteral {
  pub head: String,
  pub parts: SmallVec<(ASTIndex, String)>,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct ExprFunctionCall {
  pub callee: ASTIndex,
  pub generics: SmallVec<Type>,
  pub arguments: SmallVec<ASTIndex>,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct ExprRegexLiteral {
  pub pattern: String,
  pub flags: String,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum ASTNode {
  /// A block of code
  Block { nodes: SmallVec<ASTIndex> },

  /// A `var`, `let` or `const` variable definition
  VariableDeclaration {
    modifiers: ModifierList,
    def_type: VariableDefType,
    defs: SmallVec<DestructurableDeclaration>
  },

  StatementImport { inner: Box<ImportDefinition> },
  ExpressionImport { value: ASTIndex },

  /// Block export statement (e.g., `export { Real, type Fake }`)
  StatementExport { inner: Box<ExportDeclaration> },

  StatementIf {
    condition: ASTIndex,
    body: ASTIndex,
    alternate: Option<ASTIndex>
  },
  StatementWhile {
    condition: ASTIndex,
    body: ASTIndex,
  },
  StatementFor {
    init: ASTIndex,
    condition: ASTIndex,
    update: ASTIndex,
    body: ASTIndex,
  },
  StatementForOf {
    init: ASTIndex,
    expression: ASTIndex,
    body: ASTIndex,
  },
  StatementForIn {
    init: ASTIndex,
    expression: ASTIndex,
    body: ASTIndex,
  },

  StatementSwitch { inner: Box<Switch> },

  StatementReturn { value: Option<ASTIndex> },
  StatementBreak { value: Option<ASTIndex> },
  StatementContinue { value: Option<ASTIndex> },
  StatementThrow { value: Option<ASTIndex> },

  StatementTryCatchFinally { inner: Box<TryCatchFinally> },

  FunctionDefinition { inner: Box<FunctionDefinition> },
  ArrowFunctionDefinition { inner: Box<ArrowFunctionDefinition> },

  ClassDefinition { inner: Box<ClassDefinition> },

  // Used for expressions

  Parenthesis { nodes: SmallVec<ASTIndex> },
  Array { nodes: SmallVec<ASTIndex> },
  Dict { properties: SmallVec<ObjectProperty> },

  // Expression parsing...

  ExprIdentifier { name: String },
  ExprNumLiteral { number: String },
  ExprStrLiteral { string: String },
  ExprRegexLiteral { inner: Box<ExprRegexLiteral> },
  ExprTemplateLiteral { inner: Box<ExprTemplateLiteral> },
  ExprBoolLiteral { value: bool },

  ExprFunctionCall { inner: Box<ExprFunctionCall> },
  TemplateLiteralTag { callee: ASTIndex, argument: ASTIndex },

  ExprIndexing { callee: ASTIndex, property: ASTIndex },

  ExprTernary {
    condition: ASTIndex,
    if_true: ASTIndex,
    if_false: ASTIndex,
  },

  PrefixOpr { opr: String, expr: ASTIndex },
  InfixOpr { left_right: (ASTIndex, ASTIndex), opr: String },
  PostfixOpr { expr: ASTIndex, opr: String },
  NonNullAssertion { expr: ASTIndex },

  // Type casting

  /// Casting something with the `as` keyword (eg. `obj as any`)
  ExprAs {
    value: ASTIndex,
    cast_type: Box<Type>
  },

  /// Casting something C-style (eg. `<any>obj`)
  ExprTypeAssertion {
    cast_type: Box<Type>,
    value: ASTIndex,
  },

  EnumDeclaration { inner: Box<EnumDeclaration> },
  InterfaceDeclaration { inner: Box<InterfaceDeclaration> },

  /// `declare namespace X { ... }`
  DeclareNamespace { inner: Box<NamespaceDeclaration> },

  /// Used in situations like `[ 1, 2, ]` where there's an empty expression
  Empty
}

impl ASTNode {
  /// Gets the name of this enum, without the associated data
  pub fn name(&self) -> String {
    use ASTNode::*;
    match self {
      Block { .. } => "Block",
      VariableDeclaration { .. } => "VariableDeclaration",
      StatementImport { .. } => "StatementImport",
      ExpressionImport { .. } => "ExpressionImport",
      StatementExport { .. } => "StatementExport",
      StatementIf { .. } => "StatementIf",
      StatementWhile { .. } => "StatementWhile",
      StatementFor { .. } => "StatementFor",
      StatementForOf { .. } => "StatementForOf",
      StatementForIn { .. } => "StatementForIn",
      StatementSwitch { .. } => "StatementSwitch",
      StatementReturn { .. } => "StatementReturn",
      StatementBreak { .. } => "StatementBreak",
      StatementContinue { .. } => "StatementContinue",
      StatementThrow { .. } => "StatementThrow",
      StatementTryCatchFinally { .. } => "StatementTryCatchFinally",
      FunctionDefinition { .. } => "FunctionDefinition",
      ArrowFunctionDefinition { .. } => "ArrowFunctionDefinition",
      ClassDefinition { .. } => "ClassDefinition",
      Parenthesis { .. } => "Parenthesis",
      Array { .. } => "Array",
      Dict { .. } => "Dict",
      ExprNumLiteral { .. } => "ExprNumLiteral",
      ExprStrLiteral { .. } => "ExprStrLiteral",
      ExprRegexLiteral { .. } => "ExprRegexLiteral",
      ExprTemplateLiteral { .. } => "ExprTemplateLiteral",
      ExprIdentifier { .. } => "ExprIdentifier",
      ExprBoolLiteral { .. } => "ExprBoolLiteral",
      ExprFunctionCall { .. } => "ExprFunctionCall",
      TemplateLiteralTag { .. } => "TemplateLiteralTag",
      ExprIndexing { .. } => "ExprIndexing",
      ExprTernary { .. } => "ExprTernary",
      PrefixOpr { .. } => "PrefixOpr",
      InfixOpr { .. } => "InfixOpr",
      PostfixOpr { .. } => "PostfixOpr",
      NonNullAssertion { .. } => "NonNullAssertion",
      ExprAs { .. } => "ExprAs",
      ExprTypeAssertion { .. } => "ExprTypeAssertion",
      EnumDeclaration { .. } => "EnumDeclaration",
      InterfaceDeclaration { .. } => "InterfaceDeclaration",
      DeclareNamespace { .. } => "DeclareNamespace",
      Empty { .. } => "Empty",
    }.to_string()
  }

  /// Checks if a node is a literal
  pub fn is_literal(&self) -> bool {
    matches!(
      self,
      ASTNode::ExprNumLiteral { .. } |
      ASTNode::ExprStrLiteral { .. } |
      ASTNode::ExprRegexLiteral { .. } |
      ASTNode::ExprBoolLiteral { .. }
    )
  }

  pub fn apply_modifiers(&mut self, new_modifiers: ModifierList) -> Result<(), CompilerError> {
    use ASTNode::*;
    match self {
      VariableDeclaration { modifiers, .. } => { *modifiers = new_modifiers; }
      FunctionDefinition { inner } => { inner.modifiers = new_modifiers; }
      ClassDefinition { inner } => { inner.modifiers = new_modifiers; }
      EnumDeclaration { inner } => { inner.modifiers = new_modifiers; }
      InterfaceDeclaration { inner } => { inner.modifiers = new_modifiers; }
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

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct ASTIndex(u32);
pub struct ASTArena {
  // This is a normal `Vec` because it's only stored once!
  // `Vec` is way better optimized than our `SmallVec` implementation,
  // so it's WELL worth the 8 bytes.
  pub nodes: Vec<ASTNode>,
}
impl ASTArena {
  pub fn new() -> Self {
    ASTArena {
      nodes: Vec::with_capacity(512),
    }
  }

  // pub fn allocate_empty(&mut self) -> ASTIndex {
  //   self.add(ASTNode::Empty)
  // }

  pub fn add(&mut self, node: ASTNode) -> ASTIndex {
    let idx = self.nodes.len();
    self.nodes.push(node);
    ASTIndex(idx as u32)
  }
  pub fn get(&self, idx: ASTIndex) -> &ASTNode {
    unsafe { self.nodes.get_unchecked(idx.0 as usize) }
  }
  pub fn get_mut(&mut self, idx: ASTIndex) -> &mut ASTNode {
    unsafe { self.nodes.get_unchecked_mut(idx.0 as usize) }
  }

  pub fn set(&mut self, idx: ASTIndex, node: ASTNode) {
    unsafe { *self.nodes.get_unchecked_mut(idx.0 as usize) = node; }
  }
}
