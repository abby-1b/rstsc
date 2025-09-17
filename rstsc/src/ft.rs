use crate::{
  ast,
  ast_common::{ModifierList, VariableDefType},
  declaration::{Declaration, DestructurableDeclaration},
  operations::{get_operator_binding_power, ExprType},
  small_vec::SmallVec,
  rest::Rest,
  types::Type
};

pub fn get_operator_binding_power_from_node_typed(node: &ASTNode) -> Option<(u8, u8)> {
  if let Some(opr_type) = ExprType::from_typed(node) {
    get_operator_binding_power(opr_type, match node {
      ASTNode::PrefixOpr { opr, .. } => opr,
      ASTNode::InfixOpr { opr, .. } => opr,
      ASTNode::PostfixOpr { opr, .. } => opr,
      _ => " "
    })
  } else {
    None
  }
}

#[derive(Debug, Clone)]
pub enum ObjectProperty {
  Property {
    computed: bool,
    key: ASTNode,
    value: ASTNode
  },
  Rest {
    argument: ASTNode
  }
}

impl ObjectProperty {
  pub fn from(o: &ast::ObjectProperty) -> ObjectProperty {
    match o {
      ast::ObjectProperty::Property {
        computed,
        key,
        value
      } => ObjectProperty::Property {
        computed: *computed,
        key: ASTNode::from(key),
        value: ASTNode::from(value)
      },
      ast::ObjectProperty::Rest { argument } => ObjectProperty::Rest {
        argument: ASTNode::from(argument)
      },
      ast::ObjectProperty::Shorthand { key } => todo!()
    }
  }
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition {
  pub modifiers: ModifierList,
  pub name: Option<String>,
  pub generics: SmallVec<Type>,
  pub params: SmallVec<Declaration>,
  pub rest: Rest,
  pub return_type: Type,
  pub body: Option<Box<ASTNode>>
}

#[derive(Debug, Clone)]
pub struct ArrowFunctionDefinition {
  pub params: SmallVec<Declaration>,
  pub rest: Rest,
  pub return_type: Type,
  pub body: Box<ASTNode>
}

#[derive(Debug, Clone)]
pub enum ASTNode {
  Block { nodes: Vec<ASTNode> },

  VariableDeclaration {
    modifiers: ModifierList,
    def_type: VariableDefType,
    defs: SmallVec<DestructurableDeclaration>
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

  FunctionDefinition(FunctionDefinition),

  ArrowFunctionDefinition(ArrowFunctionDefinition),
  PotentialParameter {
    name: String,
    param_type: Type
  },

  Parenthesis { nodes: SmallVec<ASTNode> },
  Array { nodes: SmallVec<ASTNode> },
  Dict { properties: SmallVec<ObjectProperty> },

  ExprIdentifier { name: String },
  ExprNumLiteral { number: String },
  ExprStrLiteral { string: String },
  ExprBoolLiteral { value: bool },

  ExprFunctionCall { callee: Box<ASTNode>, arguments: SmallVec<ASTNode> },
  ExprIndexing { callee: Box<ASTNode>, property: Box<ASTNode> },

  ExprTernary {
    condition: Box<ASTNode>,
    if_true: Box<ASTNode>,
    if_false: Box<ASTNode>
  },

  PrefixOpr { opr: String, expr: Box<ASTNode> },
  InfixOpr { left: Box<ASTNode>, opr: String, right: Box<ASTNode> },
  PostfixOpr { expr: Box<ASTNode>, opr: String },

  ExprAs {
    value: Box<ASTNode>,
    cast_type: Type
  },

  ExprTypeAssertion {
    cast_type: Type,
    value: Box<ASTNode>,
  },

  TypeDeclaration {
    first_typ: Type,
    equals_typ: Type
  },

  Empty
}

impl ASTNode {
  /// Converts an ASTNode into its fully typed version.
  pub fn from(node: &ast::ASTNode) -> ASTNode {
    match node {
      ast::ASTNode::Block { nodes } => ASTNode::Block {
        nodes: nodes.iter().map(ASTNode::from).collect(),
      },
      ast::ASTNode::VariableDeclaration {
        modifiers,
        def_type,
        defs,
      } => ASTNode::VariableDeclaration {
        modifiers: modifiers.clone(),
        def_type: def_type.clone(),
        defs: defs.clone(),
      },
      ast::ASTNode::StatementIf {
        condition,
        body,
        alternate,
      } => ASTNode::StatementIf {
        condition: Box::new(ASTNode::from(condition)),
        body: Box::new(ASTNode::from(body)),
        alternate: alternate.as_ref().map(|a| Box::new(ASTNode::from(a))),
      },
      ast::ASTNode::StatementWhile { condition, body } => ASTNode::StatementWhile {
        condition: Box::new(ASTNode::from(condition)),
        body: Box::new(ASTNode::from(body)),
      },
      ast::ASTNode::StatementFor {
        init,
        condition,
        update,
        body,
      } => ASTNode::StatementFor {
        init: Box::new(ASTNode::from(init)),
        condition: Box::new(ASTNode::from(condition)),
        update: Box::new(ASTNode::from(update)),
        body: Box::new(ASTNode::from(body)),
      },
      ast::ASTNode::StatementReturn { value } => ASTNode::StatementReturn {
        value: value.as_ref().map(|a| Box::new(ASTNode::from(a))),
      },
      ast::ASTNode::StatementBreak { value } => ASTNode::StatementBreak {
        value: value.as_ref().map(|a| Box::new(ASTNode::from(a))),
      },
      ast::ASTNode::StatementContinue { value } => ASTNode::StatementContinue {
        value: value.as_ref().map(|a| Box::new(ASTNode::from(a))),
      },
      ast::ASTNode::StatementThrow { value } => ASTNode::StatementThrow {
        value: value.as_ref().map(|a| Box::new(ASTNode::from(a))),
      },
      ast::ASTNode::Parenthesis { nodes } => ASTNode::Parenthesis {
        nodes: nodes.iter().map(ASTNode::from).collect(),
      },
      ast::ASTNode::Array { nodes } => ASTNode::Array {
        nodes: nodes.iter().map(ASTNode::from).collect(),
      },
      ast::ASTNode::Dict { properties } => ASTNode::Dict {
        properties: properties.iter().map(ObjectProperty::from).collect(),
      },
      ast::ASTNode::ExprIdentifier { name } => ASTNode::ExprIdentifier {
        name: name.clone(),
      },
      ast::ASTNode::ExprNumLiteral { number } => ASTNode::ExprNumLiteral {
        number: number.clone(),
      },
      ast::ASTNode::ExprStrLiteral { string } => ASTNode::ExprStrLiteral {
        string: string.clone(),
      },
      ast::ASTNode::ExprBoolLiteral { value } => ASTNode::ExprBoolLiteral {
        value: *value,
      },
      ast::ASTNode::ExprFunctionCall { callee, generics: _, arguments } => ASTNode::ExprFunctionCall {
        // TODO: add generics
        callee: Box::new(ASTNode::from(callee)),
        arguments: arguments.iter().map(ASTNode::from).collect(),
      },
      ast::ASTNode::ExprIndexing { callee, property } => ASTNode::ExprIndexing {
        callee: Box::new(ASTNode::from(callee)),
        property: Box::new(ASTNode::from(property)),
      },
      ast::ASTNode::ExprTernary {
        condition,
        if_true,
        if_false,
      } => ASTNode::ExprTernary {
        condition: Box::new(ASTNode::from(condition)),
        if_true: Box::new(ASTNode::from(if_true)),
        if_false: Box::new(ASTNode::from(if_false)),
      },
      ast::ASTNode::PrefixOpr { opr, expr } => ASTNode::PrefixOpr {
        opr: opr.clone(),
        expr: Box::new(ASTNode::from(expr)),
      },
      ast::ASTNode::InfixOpr {
        left,
        opr,
        right,
      } => ASTNode::InfixOpr {
        left: Box::new(ASTNode::from(left)),
        opr: opr.clone(),
        right: Box::new(ASTNode::from(right)),
      },
      ast::ASTNode::PostfixOpr { expr, opr } => ASTNode::PostfixOpr {
        expr: Box::new(ASTNode::from(expr)),
        opr: opr.clone(),
      },
      ast::ASTNode::ExprAs { value, cast_type } => ASTNode::ExprAs {
        value: Box::new(ASTNode::from(value)),
        cast_type: (**cast_type).clone(),
      },
      ast::ASTNode::ExprTypeAssertion { cast_type, value } => ASTNode::ExprTypeAssertion {
        cast_type: (**cast_type).clone(),
        value: Box::new(ASTNode::from(value)),
      },
      ast::ASTNode::Empty => ASTNode::Empty,

      // TODO: finish all missing nodes
      missing_type => {
        todo!("Missing type: {:?}", missing_type);
      } // For debugging
    }
  }

  pub fn is_literal(&self) -> bool {
    matches!(
      self,
      ASTNode::ExprNumLiteral { .. } | ASTNode::ExprStrLiteral { .. } | ASTNode::ExprBoolLiteral { .. }
    )
  }
}

impl ExprType {
  pub fn from_typed(node: &ASTNode) -> Option<ExprType> {
    match node {
      ASTNode::PrefixOpr { .. } => Some(ExprType::Prefx),
      ASTNode::InfixOpr { .. } => Some(ExprType::Infx),
      ASTNode::PostfixOpr { .. } => Some(ExprType::Pstfx),
      _ => None
    }
  }
}
