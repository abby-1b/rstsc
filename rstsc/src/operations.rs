use phf::phf_map;

use crate::ast::ASTNode;


#[derive(Debug, Clone, Copy)]
pub enum ExprType {
  Prefx,
  Infx,
  Pstfx,

  /// Used for things that aren't really operators (like conditional `:`)
  Special
}

impl ExprType {
  pub fn from(node: &ASTNode) -> Option<ExprType> {
    match node {
      ASTNode::PrefixOpr { .. } => Some(ExprType::Prefx),
      ASTNode::InfixOpr { .. } => Some(ExprType::Infx),
      ASTNode::PostfixOpr { .. } => Some(ExprType::Pstfx),
      _ => None
    }
  }
}

pub fn get_operator_binding_power(
  expr_type: ExprType, opr: &str
) -> Option<(u8, u8)> {
  match (&expr_type, opr) {
    (ExprType::Prefx, "<>") => Some((40, 41)), // Used for C-style casts

    (ExprType::Prefx, "{") => Some((40, 41)),
    (ExprType::Prefx, "[") => Some((40, 41)),
    (ExprType::Prefx, "(") => Some((40, 41)),

    (ExprType::Infx, "." ) => Some((38, 39)),
    (ExprType::Infx, "?.") => Some((38, 39)),
    (ExprType::Infx, "[" ) => Some((38, 39)),
    (ExprType::Infx, "(" ) => Some((38, 39)),
    (ExprType::Infx, "``") => Some((38, 39)),

    (ExprType::Pstfx, "!") => Some((36, 37)), // Non-null assertion
    
    (ExprType::Prefx, "new") => Some((34, 35)),

    (ExprType::Pstfx, "++") => Some((32, 33)),
    (ExprType::Pstfx, "--") => Some((32, 33)),

    (ExprType::Prefx, "++"    ) => Some((30, 31)),
    (ExprType::Prefx, "--"    ) => Some((30, 31)),
    (ExprType::Prefx, "!"     ) => Some((30, 31)),
    (ExprType::Prefx, "~"     ) => Some((30, 31)),
    (ExprType::Prefx, "+"     ) => Some((30, 31)),
    (ExprType::Prefx, "-"     ) => Some((30, 31)),
    (ExprType::Prefx, "typeof") => Some((30, 31)),
    (ExprType::Prefx, "void"  ) => Some((30, 31)),
    (ExprType::Prefx, "delete") => Some((30, 31)),
    (ExprType::Prefx, "await" ) => Some((30, 31)),

    (ExprType::Infx, "**") => Some((29, 28)),

    (ExprType::Infx, "*") => Some((26, 27)),
    (ExprType::Infx, "/") => Some((26, 27)),
    (ExprType::Infx, "%") => Some((26, 27)),

    (ExprType::Infx, "+") => Some((24, 25)),
    (ExprType::Infx, "-") => Some((24, 25)),

    (ExprType::Infx, "<<" ) => Some((22, 23)),
    (ExprType::Infx, ">>" ) => Some((22, 23)),
    (ExprType::Infx, ">>>") => Some((22, 23)),

    (ExprType::Infx, "<"         ) => Some((20, 21)),
    (ExprType::Infx, "<="        ) => Some((20, 21)),
    (ExprType::Infx, ">"         ) => Some((20, 21)),
    (ExprType::Infx, ">="        ) => Some((20, 21)),
    (ExprType::Infx, "in"        ) => Some((20, 21)),
    (ExprType::Infx, "instanceof") => Some((20, 21)),

    (ExprType::Infx, "==" ) => Some((18, 19)),
    (ExprType::Infx, "!=" ) => Some((18, 19)),
    (ExprType::Infx, "===") => Some((18, 19)),
    (ExprType::Infx, "!==") => Some((18, 19)),

    (ExprType::Infx, "&" ) => Some((16, 17)),
    (ExprType::Infx, "^" ) => Some((14, 15)),
    (ExprType::Infx, "|" ) => Some((12, 13)),
    (ExprType::Infx, "&&") => Some((10, 11)),

    (ExprType::Infx, "||") => Some((8, 9)),
    (ExprType::Infx, "??") => Some((8, 9)),

    (ExprType::Special, ":") => Some((6, 7)), // Conditional

    (ExprType::Infx,  "="   ) => Some((5, 4)),
    (ExprType::Infx,  "+="  ) => Some((5, 4)),
    (ExprType::Infx,  "-="  ) => Some((5, 4)),
    (ExprType::Infx,  "**=" ) => Some((5, 4)),
    (ExprType::Infx,  "*="  ) => Some((5, 4)),
    (ExprType::Infx,  "/="  ) => Some((5, 4)),
    (ExprType::Infx,  "%="  ) => Some((5, 4)),
    (ExprType::Infx,  "<<=" ) => Some((5, 4)),
    (ExprType::Infx,  ">>=" ) => Some((5, 4)),
    (ExprType::Infx,  ">>>=") => Some((5, 4)),
    (ExprType::Infx,  "&="  ) => Some((5, 4)),
    (ExprType::Infx,  "^="  ) => Some((5, 4)),
    (ExprType::Infx,  "|="  ) => Some((5, 4)),
    (ExprType::Infx,  "&&=" ) => Some((5, 4)),
    (ExprType::Infx,  "||=" ) => Some((5, 4)),
    (ExprType::Infx,  "??=" ) => Some((5, 4)),
    (ExprType::Infx,  "?"   ) => Some((5, 4)),
    (ExprType::Infx,  "=>"  ) => Some((5, 4)),

    (ExprType::Prefx, "yield" ) => Some((2, 3)),
    (ExprType::Prefx, "..."   ) => Some((2, 3)),

    (ExprType::Infx, ",") => Some((0, 1)),

    _ => { None }
  }
}

pub fn get_type_operator_binding_power(
  expr_type: ExprType, opr: &str
) -> Option<(u8, u8)> {
  match (&expr_type, opr) {

    (ExprType::Prefx, "{") => Some((16, 17)),
    (ExprType::Prefx, "(") => Some((16, 17)),
    (ExprType::Prefx, "<") => Some((16, 17)),
    (ExprType::Prefx, "[") => Some((16, 17)),

    (ExprType::Infx, ".") => Some((14, 15)),
    (ExprType::Infx, "[") => Some((14, 15)),
    (ExprType::Infx, "<") => Some((14, 15)),

    (ExprType::Prefx, "-"  ) => Some((12, 13)),
    (ExprType::Prefx, "new") => Some((12, 13)),

    (ExprType::Prefx, "typeof"  ) => Some((10, 11)),
    (ExprType::Prefx, "keyof"   ) => Some((10, 11)),
    (ExprType::Prefx, "asserts" ) => Some((10, 11)),
    (ExprType::Prefx, "infer"   ) => Some((10, 11)),
    (ExprType::Prefx, "readonly") => Some((10, 11)),
    (ExprType::Prefx, "unique"  ) => Some((10, 11)),
    (ExprType::Prefx, "abstract") => Some((10, 11)),
    (ExprType::Prefx, "&") => Some((10, 11)),
    (ExprType::Prefx, "|") => Some((10, 11)),

    (ExprType::Infx , "&") => Some((8, 9)),

    (ExprType::Infx , "|") => Some((6, 7)),

    (ExprType::Infx, "extends") => Some((4, 5)),

    (ExprType::Infx , "?"  ) => Some((2, 3)), // Conditional
    (ExprType::Infx , "is" ) => Some((2, 3)),
    (ExprType::Prefx, "...") => Some((2, 3)), // Spread

    (ExprType::Infx, ":" ) => Some((0, 1)),

    _ => { None }
  }
}

pub fn get_operator_binding_power_from_node(node: &ASTNode) -> Option<(u8, u8)> {
  if let Some(opr_type) = ExprType::from(node) {
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

lazy_static::lazy_static! {
  pub static ref ARROW_FN_PRECEDENCE: u8 = get_operator_binding_power(ExprType::Infx, "=>").unwrap().1;
  pub static ref COLON_PRECEDENCE: u8 = get_operator_binding_power(ExprType::Special, ":").unwrap().1;
  pub static ref COMMA_PRECEDENCE: u8 = get_operator_binding_power(ExprType::Infx, ",").unwrap().1;
  pub static ref TEMPLATE_LITERAL_TAG_PRECEDENCE: u8 = get_operator_binding_power(ExprType::Infx, "``").unwrap().0;
}
static MODIFYING_KW: phf::Map<&'static str, ExprType> = phf_map! {
  "="    => ExprType::Infx,
  "+="   => ExprType::Infx,
  "-="   => ExprType::Infx,
  "**="  => ExprType::Infx,
  "*="   => ExprType::Infx,
  "/="   => ExprType::Infx,
  "%="   => ExprType::Infx,
  "<<="  => ExprType::Infx,
  ">>="  => ExprType::Infx,
  ">>>=" => ExprType::Infx,
  "&="   => ExprType::Infx,
  "^="   => ExprType::Infx,
  "|="   => ExprType::Infx,
  "&&="  => ExprType::Infx,
  "||="  => ExprType::Infx,
  "??="  => ExprType::Infx,
};

/// If this operator is modifying, returns Some() with the operator's type
/// If it isn't, returns None.
#[inline]
pub fn is_modifying(opr: &str) -> Option<ExprType> {
  MODIFYING_KW.get(opr).copied()
}
