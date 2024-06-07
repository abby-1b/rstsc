use crate::{ft::ASTNode, types::{combine_types, Type}};

// const ASSIGNMENT_OPERATORS: &[&str] = &[
//     "=", "+=", "-=", "**=", "*=", "/=", "%=", "<<=", ">>=", ">>>=", "&=", "^=",
//     "|=", "&&=", "||=", "??=",
// ];

/// A scope contains variables, their types, and their values
struct Scope<'a> {
    parent: &'a Scope<'a>,
}

/// Infers a type from an AST node, as specifically as possible
fn infer_type_from_node(node: &ASTNode) -> Type {
    match node {
        ASTNode::ExprNumLiteral { number } => Type::Number,
        ASTNode::ExprStrLiteral { .. } => Type::String,
        ASTNode::ExprBoolLiteral { .. } => Type::Boolean,
        ASTNode::Parenthesis { nodes } => {
            let last_node = nodes.last();
            if let Some(last_node) = last_node {
                infer_type_from_node(last_node)
            } else {
                panic!("Expected expression, found empty parenthesis.");
            }
        },
        ASTNode::Array { nodes } => {
            Type::Array(Box::new(
                if nodes.is_empty() {
                    Type::Unknown
                } else {
                    nodes
                        .iter()
                        .map(infer_type_from_node)
                        .reduce(|a, b| combine_types(&a, &b))
                        .unwrap_or(Type::Unknown)
                }
            ))
        },
        ASTNode::InfixOpr { left: _, opr, right } => {
            let opr_str = opr.as_str();
            if opr_str == "=" {
                infer_type_from_node(&*right)
            } else {
                // TODO: properly infer (eg. `a += 1` might be string or number, depending on the type of A)
                Type::Unknown
            }
        },
        _ => { Type::Unknown }
    }
}

/// Infers the return type of a function, and annotates it with the type
pub fn infer_return_type(node: &mut ASTNode) {
    match node {
        ASTNode::FunctionDefinition {
            modifiers,
            name,
            generics,
            params,
            return_type,
            body
        } => {

        }
        ASTNode::ArrowFunctionDefinition {
            params,
            return_type,
            body
        } => {
            // TODO: make this work
            if let ASTNode::Block { nodes } = &**body {
                // Body is a block
            } else {
                // Body is a single expression
                infer_type_from_node(body);
            }
        }
        other => {
            panic!("Tried inferring non-function node: {:?}", node);
        }
    }
}