
use crate::{
  ast::{ASTNode, ObjectProperty},
  declaration::DeclarationTyped,
  small_vec::SmallVec,
  symbol_table::SymbolTable,
  types::{CustomDouble, Type}
};

pub fn widen_type(typ: &Type) -> Type {
  match typ {
    Type::NumberLiteral(..) => Type::Number,
    Type::StringLiteral(..) => Type::Number,
    Type::BooleanLiteral(..) => Type::Number,
    Type::Tuple { inner_types } => {
      let mut inner = SmallVec::new();
      for t in inner_types {
        inner.push(t.0.clone());
      }
      Type::Array(Box::new(Type::union_from(&inner)))
    },
    _ => typ.clone()
  }
}

// Helper rule functions

fn recurse_all(nodes: &SmallVec<ASTNode>, st: &mut SymbolTable) {
  for node in nodes.iter() {
    infer_types_with_symbol_table(node, st);
  }
}

fn numeric_result(_: &Type, _: &Type) -> Type { Type::Number }
fn boolean_result(_: &Type, _: &Type) -> Type { Type::Boolean }

fn union_result(mut a: Type, b: Type) -> Type {
  a.union(b);
  a
}

// TODO: make this modify the symbol table to widen or modify types
fn infix_result(op: &str, l: Type, r: Type) -> Type {
  match op {
    "=" => r,
    "+=" | "-=" | "*=" | "/=" | "%=" | "**=" | "<<=" | ">>=" | ">>>=" | "&=" | "^=" | "|=" => l,
    "+" => match (&l, &r) {
      (Type::String, _) | (_, Type::String) => Type::String,
      _ => Type::Number,
    },
    "-" | "*" | "/" | "%" | "**" | "<<" | ">>" | ">>>" => numeric_result(&l, &r),
    "==" | "!=" | "===" | "!==" | "<" | ">" | "<=" | ">=" | "instanceof" | "in" => boolean_result(&l, &r),
    "&&" | "||" | "??" => union_result(l, r),
    "?." => union_result(l, Type::Void),
    _ => Type::Unknown,
  }
}

fn prefix_result(op: &str, t: Type) -> Type {
  match op {
    "+" | "-" | "~" => Type::Number,
    "!" => Type::Boolean,
    "typeof" => Type::String,
    "void" => Type::Void,
    "delete" => Type::Boolean,
    "await" => match t {
      Type::WithArgs(inner, _) => *inner,
      _ => Type::Unknown,
    },
    _ => Type::Unknown,
  }
}

fn postfix_result(op: &str, t: Type) -> Type {
  match op {
    "!" => { let mut t = t.clone(); t.remove(Type::Void); t },
    "++" => Type::Number,
    "--" => Type::Number,
    _ => Type::Unknown,
  }
}

fn index_result(arr: &Type, index: &Type) -> Type {
  match (arr, index) {
    (Type::Array(of_type), Type::Number | Type::NumberLiteral(..)) => {
      (**of_type).clone()
    },
    (Type::Tuple { inner_types }, Type::Number) => {
      if inner_types.len() == 0 {
        Type::Void
      } else {
        let mut union_type = inner_types[0].0.clone();
        for i in 1..inner_types.len() {
          union_type.union(inner_types[i].0.clone());
        }
        union_type
      }
    },
    (Type::Tuple { inner_types }, Type::NumberLiteral(index)) => {
      if inner_types.len() == 0 || unsafe {
        index.value.fract() != 0.0 || index.value < 0.0
      } {
        Type::Void
      } else {
        let index = unsafe { index.value } as usize;
        let mut union_type = Type::Unknown;
        for i in 0..inner_types.len() {
          let t = inner_types[i].0.clone();
          if index == i { return t; }
          union_type.union(t.clone());
        }
        union_type
      }
    },
    _ => Type::Unknown,
  }
}

// Main inference functions

pub fn infer_types_with_symbol_table(node: &ASTNode, st: &mut SymbolTable) -> Type {
  use ASTNode::*;

  match node {
    Block { nodes } => {
      recurse_all(nodes, st);
      Type::Void
    },

    ExprIdentifier { name } => {
      if let Some(symbol) = st.lookup(name) {
        symbol.typ.clone()
      } else {
        Type::Unknown
      }
    },
    ExprNumLiteral { number } => Type::NumberLiteral(CustomDouble::from_str(&number).unwrap()),
    ExprStrLiteral { string } => Type::StringLiteral(string.clone()),
    ExprRegexLiteral { .. } => Type::RegExp,
    ExprTemplateLiteral { .. } => Type::String,
    ExprFunctionCall { callee, generics, arguments } => Type::Unknown,
    ExprBoolLiteral { value } => Type::BooleanLiteral(*value),

    ExprIndexing { callee, property } => index_result(
      &infer_types_with_symbol_table(callee, st),
      &infer_types_with_symbol_table(property, st),
    ),
    ExprTernary { condition, if_true, if_false } => {
      let cond_t = infer_types_with_symbol_table(condition, st);
      let a = infer_types_with_symbol_table(if_true, st);
      let b = infer_types_with_symbol_table(if_false, st);
      match cond_t {
        Type::BooleanLiteral(true) => a,
        Type::BooleanLiteral(false) => b,
        _ => union_result(a, b)
      }
    },

    PrefixOpr { opr, expr } => {
      let t = infer_types_with_symbol_table(expr, st);
      let r = prefix_result(opr.as_str(), t);
      r
    },

    InfixOpr { left, opr, right } => {
      let l = infer_types_with_symbol_table(left, st);
      let r = infer_types_with_symbol_table(right, st);
      let res = infix_result(opr.as_str(), l, r);
      res
    },

    PostfixOpr { expr, opr } => {
      let t = infer_types_with_symbol_table(expr, st);
      let r = postfix_result(opr.as_str(), t);
      r
    },

    Parenthesis { nodes } => {
      if nodes.len() == 0 {
        Type::Void
      } else {
        let mut last: Option<Type> = None;
        for n in nodes.iter() {
          last = Some(infer_types_with_symbol_table(n, st));
        }
        last.unwrap()
      }
    },

    Array { nodes } => {
      let mut types = SmallVec::new();
      for n in nodes {
        types.push(infer_types_with_symbol_table(n, st));
      }
      Type::union_from(&types)
    },

    Dict { properties } => {
      let mut parts = SmallVec::new();
      for p in properties {
        match p {
          ObjectProperty::Property { computed, key, value } => {
            let typ = infer_types_with_symbol_table(value, st);
            match (*computed, key) {
              (false, ExprIdentifier { name }) => parts.push(
                DeclarationTyped::named(name.clone(), typ)
              ),
              _ => {
                parts.push(DeclarationTyped::computed(key.clone(), typ))
              }
            }
          },
          ObjectProperty::Rest { argument } => {
            // TODO: rest type inference
          },
          ObjectProperty::Shorthand { key } => {
            // TODO: shorthand type inference
          },
        }
      }
      Type::Object { key_value: SmallVec::new(), parts }
    },

    StatementReturn { value } => {
      if let Some(e) = value {
        infer_types_with_symbol_table(e, st)
      } else {
        Type::Void
      }
    },

    // TODO: infer missing types
    _ => Type::Unknown
  }
}

fn infer_return_type(body: &ASTNode, st: &mut SymbolTable) -> Type {
  match body {
    ASTNode::Block { nodes } => {
      // find returns. if none, void
      let mut acc: Option<Type> = None;
      for stmt in nodes.iter() {
        match stmt {
          ASTNode::StatementReturn { value } => {
            let t = if let Some(e) = value { infer_types_with_symbol_table(e, st) } else { Type::Void };
            if let Some(prev) = acc.as_mut() { prev.union(t); } else { acc = Some(t); }
          },
          ASTNode::Block { .. } | ASTNode::ExprFunctionCall { .. } | ASTNode::InfixOpr { .. } | ASTNode::PrefixOpr { .. } | ASTNode::PostfixOpr { .. }=> {
            // still walk nested statements to widen/modify symbol table types
            infer_types_with_symbol_table(stmt, st);
          },
          _ => { infer_types_with_symbol_table(stmt, st); }
        }
      }
      acc.unwrap_or(Type::Void)
    }
    _ => infer_types_with_symbol_table(body, st),
  }
}
