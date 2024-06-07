use crate::{ast::ASTNode, ft};


#[derive(Debug)]
pub enum ExprType {
    Prefx,
    Infx,
    Pstfx,
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
    pub fn from_typed(node: &ft::ASTNode) -> Option<ExprType> {
        match node {
            ft::ASTNode::PrefixOpr { .. } => Some(ExprType::Prefx),
            ft::ASTNode::InfixOpr { .. } => Some(ExprType::Infx),
            ft::ASTNode::PostfixOpr { .. } => Some(ExprType::Pstfx),
            _ => None
        }
    }
}

pub fn get_operator_binding_power(
    expr_type: ExprType, opr: &str
) -> Option<(u8, u8)> {
    // TODO: add TypeScript's `!` postfix operator
    match (&expr_type, opr) {
        (ExprType::Prefx, "{") => Some((38, 39)),
        (ExprType::Prefx, "[") => Some((38, 39)),
        (ExprType::Prefx, "(") => Some((38, 39)),

        (ExprType::Infx, "." ) => Some((36, 37)),
        (ExprType::Infx, "?.") => Some((36, 37)),
        (ExprType::Infx, "[" ) => Some((36, 37)),
        (ExprType::Infx, "(" ) => Some((36, 37)),
        
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

        (ExprType::Infx, ":") => Some((6, 7)), // Type declarations

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

        (ExprType::Prefx, "yield") => Some((2, 3)),
        (ExprType::Prefx, "..."  ) => Some((2, 3)),

        (ExprType::Infx, ",") => Some((0, 1)),

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

pub fn get_operator_binding_power_from_node_typed(node: &ft::ASTNode) -> Option<(u8, u8)> {
    if let Some(opr_type) = ExprType::from_typed(node) {
        get_operator_binding_power(opr_type, match node {
            ft::ASTNode::PrefixOpr { opr, .. } => opr,
            ft::ASTNode::InfixOpr { opr, .. } => opr,
            ft::ASTNode::PostfixOpr { opr, .. } => opr,
            _ => " "
        })
    } else {
        None
    }
}
