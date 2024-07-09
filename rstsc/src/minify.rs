use crate::{ast, ft::{self, ASTNode, ObjectProperty}, operations::{get_operator_binding_power, get_operator_binding_power_from_node_typed, ExprType}};

/// Minifies an AST node, modifying it in the process.
pub fn minify_ast(node: &mut ast::ASTNode) {
    // Do full type inferring
    let mut node = ft::ASTNode::from(node);

    // Combine nodes, evaluating constants and shrinking things down
    step_combine(&mut node);

    // TODO: Variable stripping
}

fn step_combine(node: &mut ASTNode) {
    match node {
        ASTNode::Block { nodes } => {
            nodes.iter_mut().for_each(step_combine);
            // TODO: combine variable declarations
        }
        ASTNode::VariableDeclaration { defs, .. } => {
            defs.iter_mut().for_each(|def| {
                def.value.as_mut().map(step_combine);
            });
        }
        ASTNode::StatementIf { condition, body, alternate } => {
            // Combine children
            step_combine(condition);
            step_combine(body);
            alternate.as_deref_mut().map(step_combine);

            // TODO: move this into a later step! vvv

            // Take out of block (if possible)
            take_out_of_block(body);
            alternate.as_mut().map(take_out_of_block);

            match (&**body, alternate.as_deref_mut()) {
                (
                    &ASTNode::StatementReturn { value: Some(ref return_true) },
                    Some(ASTNode::StatementReturn { value: Some(ref return_false) })
                ) => {
                    // TODO: fix precedence with inline conditions
                    let new_node = ASTNode::StatementReturn {
                        value: Some(Box::new(ASTNode::ExprTernary {
                            condition: condition.clone(),
                            if_true: return_true.clone(),
                            if_false: return_false.clone()
                        }))
                    };
                    replace_node(node, new_node)
                }
                _ => {}
            }
        }
        // ASTNode::StatementWhile { .. } => {}
        // ASTNode::StatementFor { .. } => {}
        ASTNode::StatementReturn { value } => {
            value.as_deref_mut().map(step_combine);
        }
        // ASTNode::StatementBreak { .. } => {}
        // ASTNode::StatementContinue { .. } => {}
        ASTNode::FunctionDefinition { body, .. } => {
            // TODO: minify parameter defaults
            body.as_deref_mut().map(step_combine);
        }
        // ASTNode::ArrowFunctionDefinition { .. } => {}
        // ASTNode::PotentialParameter { .. } => {}
        ASTNode::Parenthesis { nodes } => {
            // Minify children
            nodes.iter_mut().for_each(step_combine);

            if nodes.len() == 1 {
                // Take care of parenthesis within parenthesis
                if matches!(nodes[0], ASTNode::Parenthesis { .. }) {
                    let inner_paren = unsafe { nodes.pop().unwrap_unchecked() };
                    replace_node(node, inner_paren);
                }
            } else {
                // TODO: take care of `(1, 2)` => `(2)`
            }
        }
        // ASTNode::Array { .. } => {}
        ASTNode::Dict { properties } => {
            properties.iter_mut().for_each(|property| {
                match property {
                    ObjectProperty::Property { computed, key, value } => {
                        if *computed {
                            step_combine(key);
                        }
                        step_combine(value);
                    }
                    ObjectProperty::Spread { argument } => step_combine(argument)
                }
            });
        }
        ASTNode::ExprNumLiteral { .. } => {}
        ASTNode::ExprStrLiteral { .. } => {}
        ASTNode::ExprBoolLiteral { .. } => {}
        ASTNode::ExprIdentifier { .. } => {}
        ASTNode::ExprFunctionCall { callee, arguments } => {
            // Minify parts
            step_combine(callee);
            arguments.iter_mut().for_each(step_combine);

            if let ASTNode::Parenthesis { nodes } = &**callee {
                if nodes.len() == 1 {
                    if let ASTNode::FunctionDefinition { modifiers, name, generics, params, return_type, body } = &nodes[0] {

                    }
                }
            }
        }
        // ASTNode::ExprIndexing { .. } => {}
        ASTNode::PrefixOpr { opr, expr } => {
            step_combine(expr);
        }
        ASTNode::InfixOpr { left, opr, right } => {
            // Combine children
            step_combine(left);
            step_combine(right);

            // Remove inset parenthesis
            for side in [ &mut *left, &mut *right ] {
                if let ASTNode::Parenthesis { nodes } = &mut **side {
                    if nodes.len() == 1 {
                        let side_power = get_operator_binding_power_from_node_typed(&nodes[0]);
                        let should_replace = side_power.is_none() || side_power > get_operator_binding_power(ExprType::Infx, opr);
                        if should_replace {
                            let inner_node = unsafe { nodes.pop().unwrap_unchecked() };
                            replace_node(&mut *side, inner_node);
                        }
                    }
                }
            }

            // Combine literals
            if left.is_literal() && right.is_literal() {
                let new_node = match opr.as_str() {
                    "+" => combine_add(left, right),
                    "-" => combine_sub(left, right),
                    "*" => combine_mul(left, right),
                    _ => None
                };
                if let Some(new_node) = new_node {
                    replace_node(node, new_node);
                }
            }
        }
        ASTNode::PostfixOpr { expr, .. } => { step_combine(expr); }
        ASTNode::ExprAs { value, .. } => {
            step_combine(value);
        }
        // ASTNode::ExprTypeAssertion { .. } => {}
        ASTNode::TypeDeclaration { .. } => {}
        ASTNode::Empty { .. } => {},
        node => {
            panic!("Not implemented in combine step: {:?}", node);
        }
    }
}

fn combine_add(left: &ASTNode, right: &ASTNode) -> Option<ASTNode> {
    Some(match (left, right) {
        (ASTNode::ExprNumLiteral { number: lhs }, ASTNode::ExprNumLiteral { number: rhs }) => {
            let lhs = lhs.parse::<f64>().unwrap_or(f64::NAN);
            let rhs = rhs.parse::<f64>().unwrap_or(f64::NAN);
            ASTNode::ExprNumLiteral { number: (lhs + rhs).to_string() }
        }
        (ASTNode::ExprNumLiteral { number: lhs }, ASTNode::ExprStrLiteral { string: rhs }) => {
            let quotation = &rhs[0..1];
            ASTNode::ExprStrLiteral { string: String::new() + quotation + lhs + &rhs[1..rhs.len() - 1] + quotation }
        }
        (ASTNode::ExprNumLiteral { number: lhs }, ASTNode::ExprBoolLiteral { value: rhs }) => {
            let lhs = lhs.parse::<f64>().unwrap_or(f64::NAN);
            ASTNode::ExprNumLiteral { number: (lhs + *rhs as u8 as f64).to_string() }
        }

        (ASTNode::ExprStrLiteral { string: lhs }, ASTNode::ExprNumLiteral { number: rhs }) => {
            let quotation = &lhs[0..1];
            ASTNode::ExprStrLiteral { string: String::new() + &lhs[0..lhs.len() - 1] + &rhs + quotation }
        }
        (ASTNode::ExprStrLiteral { string: lhs }, ASTNode::ExprStrLiteral { string: rhs }) => {
            ASTNode::ExprStrLiteral { string: String::new() + &lhs[0..lhs.len() - 1] + &rhs[1..rhs.len()] }
        }
        (ASTNode::ExprStrLiteral { string: lhs }, ASTNode::ExprBoolLiteral { value: rhs }) => {
            let quotation = &lhs[0..1];
            ASTNode::ExprStrLiteral { string: String::new() + &lhs[0..lhs.len() - 1] + &rhs.to_string() + quotation }
        }

        (ASTNode::ExprBoolLiteral { value: lhs }, ASTNode::ExprNumLiteral { number: rhs }) => {
            let rhs = rhs.parse::<f64>().unwrap_or(f64::NAN);
            ASTNode::ExprStrLiteral { string: (*lhs as u8 as f64 + rhs).to_string() }
        }
        (ASTNode::ExprBoolLiteral { value: lhs }, ASTNode::ExprStrLiteral { string: rhs }) => {
            let quotation = &rhs[0..1];
            ASTNode::ExprStrLiteral { string: lhs.to_string() + quotation + &rhs[1..rhs.len() - 1] + quotation }
        }
        (ASTNode::ExprBoolLiteral { value: lhs }, ASTNode::ExprBoolLiteral { value: rhs }) => {
            ASTNode::ExprNumLiteral { number: (*lhs as u8 + *rhs as u8).to_string() }
        }

        _ => { panic!("Can't combine nodes:\n{:?}\n{:?}", left, right); }
    })
}

fn combine_sub(left: &ASTNode, right: &ASTNode) -> Option<ASTNode> {
    // TODO: fix string subtraction (quotes)
    match (left, right) {
        (ASTNode::ExprNumLiteral { number: lhs }, ASTNode::ExprNumLiteral { number: rhs }) => {
            let lhs = lhs.parse::<f64>().unwrap_or(f64::NAN);
            let rhs = rhs.parse::<f64>().unwrap_or(f64::NAN);
            Some(ASTNode::ExprNumLiteral { number: (lhs - rhs).to_string() })
        }
        (ASTNode::ExprNumLiteral { number: lhs }, ASTNode::ExprStrLiteral { .. }) => {
            Some(ASTNode::ExprStrLiteral { string: lhs.clone() })
        }
        (ASTNode::ExprNumLiteral { number: lhs }, ASTNode::ExprBoolLiteral { value: rhs }) => {
            let lhs = lhs.parse::<f64>().unwrap_or(f64::NAN);
            Some(ASTNode::ExprNumLiteral { number: (lhs - *rhs as u8 as f64).to_string() })
        }

        (ASTNode::ExprStrLiteral { string: lhs }, ASTNode::ExprNumLiteral { .. }) => {
            Some(ASTNode::ExprStrLiteral { string: lhs.clone() })
        }
        (ASTNode::ExprStrLiteral { .. }, ASTNode::ExprStrLiteral { .. }) => {
            None
        }
        (ASTNode::ExprStrLiteral { .. }, ASTNode::ExprBoolLiteral { .. }) => {
            None
        }
        
        (ASTNode::ExprBoolLiteral { value: lhs }, ASTNode::ExprNumLiteral { number: rhs }) => {
            let rhs = rhs.parse::<f64>().unwrap_or(f64::NAN);
            Some(ASTNode::ExprNumLiteral { number: (*lhs as u8 as f64 - rhs).to_string() })
        }
        (ASTNode::ExprBoolLiteral { .. }, ASTNode::ExprStrLiteral { .. }) => {
            None
        }
        (ASTNode::ExprBoolLiteral { value: lhs }, ASTNode::ExprBoolLiteral { value: rhs }) => {
            Some(ASTNode::ExprNumLiteral { number: (*lhs as u8 - *rhs as u8).to_string() })
        }

        _ => panic!("Can't combine nodes:\n{:?}\n{:?}", left, right),
    }
}

fn combine_mul(left: &ASTNode, rhs: &ASTNode) -> Option<ASTNode> {
    // TODO: fix string multiplication (quotes)
    match (left, rhs) {
        (ASTNode::ExprNumLiteral { number: lhs }, ASTNode::ExprNumLiteral { number: rhs }) => {
            let lhs = lhs.parse::<f64>().unwrap_or(f64::NAN);
            let rhs = rhs.parse::<f64>().unwrap_or(f64::NAN);
            Some(ASTNode::ExprNumLiteral { number: (lhs * rhs).to_string() })
        }
        (ASTNode::ExprNumLiteral { number: lhs }, ASTNode::ExprStrLiteral { string: rhs }) => {
            let lhs = lhs.parse::<f64>().unwrap_or(f64::NAN);
            let rhs = if rhs.is_empty() {
                0.0
            } else {
                rhs.parse::<f64>().unwrap_or(f64::NAN)
            };
            Some(ASTNode::ExprStrLiteral { string: (lhs * rhs).to_string() })
        }
        (ASTNode::ExprNumLiteral { number: lhs }, ASTNode::ExprBoolLiteral { value: rhs }) => {
            let lhs = lhs.parse::<f64>().unwrap_or(f64::NAN);
            Some(ASTNode::ExprNumLiteral { number: (lhs * *rhs as u8 as f64).to_string() })
        }

        (ASTNode::ExprStrLiteral { string: lhs }, ASTNode::ExprNumLiteral { number: rhs }) => {
            let lhs = if lhs.is_empty() {
                0.0
            } else {
                lhs.parse::<f64>().unwrap_or(f64::NAN)
            };
            let rhs = rhs.parse::<f64>().unwrap_or(f64::NAN);
            Some(ASTNode::ExprStrLiteral { string: (lhs * rhs).to_string() })
        }
        (ASTNode::ExprStrLiteral { .. }, ASTNode::ExprStrLiteral { .. }) => None,
        (ASTNode::ExprStrLiteral { string: lhs }, ASTNode::ExprBoolLiteral { value: rhs }) => {
            let lhs = if lhs.is_empty() {
                0.0
            } else {
                lhs.parse::<f64>().unwrap_or(f64::NAN)
            };
            Some(ASTNode::ExprNumLiteral { number: (lhs * *rhs as u8 as f64).to_string() })
        },

        (ASTNode::ExprBoolLiteral { value: lhs }, ASTNode::ExprNumLiteral { number: rhs }) => {
            let rhs = rhs.parse::<f64>().unwrap_or(f64::NAN);
            Some(ASTNode::ExprNumLiteral { number: (*lhs as u8 as f64 * rhs).to_string() })
        }
        (ASTNode::ExprBoolLiteral { value: lhs }, ASTNode::ExprStrLiteral { string: rhs }) => {
            let rhs = if rhs.is_empty() {
                0.0
            } else {
                rhs.parse::<f64>().unwrap_or(f64::NAN)
            };
            Some(ASTNode::ExprNumLiteral { number: (*lhs as u8 as f64 * rhs).to_string() })
        },
        (ASTNode::ExprBoolLiteral { value: lhs }, ASTNode::ExprBoolLiteral { value: rhs }) => {
            Some(ASTNode::ExprNumLiteral { number: (*lhs as u8 * *rhs as u8).to_string() })
        }
        _ => None,
    }
}

/// Replaces a node with another
fn replace_node(old_node: &mut ASTNode, new_node: ASTNode) {
    *old_node = new_node;
}

/// Simplifies a block, if possible
fn take_out_of_block(part: &mut Box<ASTNode>) {
    if let ASTNode::Block { nodes } = &mut **part {
        if nodes.len() == 1 {
            let inner_node = nodes.pop().unwrap();
            replace_node(part, inner_node);
        }
    }
}
