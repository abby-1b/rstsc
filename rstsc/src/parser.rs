use crate::{
    ast::{ASTNode, Modifier, ModifierList, NamedDeclaration, ObjectProperty, VariableDefType, MODIFIERS},
    operations::{get_operator_binding_power, ExprType},
    tokenizer::{TokenList, TokenType},
    types::get_type
};

/// Parses a single block. Consumes the ending token!
pub fn get_block(tokens: &mut TokenList) -> Result<ASTNode, String> {
    let mut nodes = vec![];

    // Go through the tokens list
    loop {
        let new_node = get_single_statement(tokens)?;
        nodes.push(new_node);

        if tokens.peek_str() == ";" { tokens.skip_unchecked(); continue; }
        if tokens.is_done() { break; }
        if [ ")", "]", "}" ].contains(&tokens.peek_str()) { tokens.skip_unchecked(); break; }
    }

    // Remove trailing empty
    if let Some(node) = nodes.last() {
        if matches!(node, ASTNode::Empty) {
            nodes.pop();
        }
    }

    // Return the program node
    Ok(ASTNode::Block { nodes })
}

/// Gets a single statement, until it reaches either a `;` or a group end
/// (eg. `)]}`). Only consumes semicolons, not group ends. If a group start is
/// reached (eg. `([{`), calls the corresponding function for said group.
fn get_single_statement(tokens: &mut TokenList) -> Result<ASTNode, String> {
    let mut nodes = vec![];

    // Go through the tokens list
    loop {
        if !nodes.is_empty() { break; }
        // Ignore whitespace
        tokens.ignore_whitespace();

        // Exit conditions
        if
            tokens.is_done() ||
            tokens.peek_str() == ";" ||
            [ ")", "]", "}" ].contains(&tokens.peek_str())
        {
            break;
        }
        
        // Handlers
        // Note that handlers don't consume `;`!
        if
            handle_blocks(tokens, &mut nodes)? ||
            handle_vars(tokens, &mut nodes)? ||
            handle_control_flow(tokens, &mut nodes)? ||
            handle_function_declaration(tokens, &mut nodes)? ||
            handle_modifiers(tokens, &mut nodes)? ||
            handle_other_statements(tokens, &mut nodes)? ||
            handle_type_declaration(tokens, &mut nodes)?
        {
            continue;
        }

        // Expression fallback
        // println!("Fallback to expression for {:?}", tokens.peek());
        handle_expression(tokens, &mut nodes)?; // Fallback
    }

    tokens.ignore_whitespace();
    if tokens.peek_str() == ";" {
        tokens.skip_unchecked();
    }

    // Return the single statement node
    Ok(nodes.pop().unwrap_or(ASTNode::Empty))
}

/// Handles blocks in the middle of nowhere
fn handle_blocks(
    tokens: &mut TokenList,
    out: &mut Vec<ASTNode>,
) -> Result<bool, String> {
    if tokens.peek_str() != "{" {
        return Ok(false);
    }

    tokens.skip_unchecked();
    out.push(get_block(tokens)?);

    Ok(true)
}

/// Handles variable initialization
fn handle_vars(
    tokens: &mut TokenList,
    out: &mut Vec<ASTNode>,
) -> Result<bool, String> {
    const VARIABLE_DECLARATIONS: &[&str] = &[ "var", "let", "const" ];
    if !VARIABLE_DECLARATIONS.contains(&tokens.peek_str()) {
        return Ok(false);
    }

    // Get the header
    let def_type = match tokens.consume().value {
        "var"   => VariableDefType::Var,
        "let"   => VariableDefType::Let,
        "const" => VariableDefType::Const,
        other => {
            return Err(format!("Unexpected var declaration: {}", other));
        }
    };

    // Get the variable definitions
    let defs = get_typed_declarations(tokens)?;

    out.push(ASTNode::VariableDeclaration {
        modifiers: Default::default(),
        def_type,
        defs
    });

    Ok(true)
}

/// Gets typed declarations, with or without values
/// 
/// Examples (in square brackets):
/// 
/// `let [a: number = 123, b: string];`
/// 
/// `function some([a: number, b: string = '123']) { ... }`
fn get_typed_declarations(
    tokens: &mut TokenList
) -> Result<Vec<NamedDeclaration>, String> {
    let declaration_node = get_expression(tokens, 0)?;
    convert_to_typed_declarations(declaration_node)
}

fn convert_to_typed_declarations(
    node: ASTNode,
) -> Result<Vec<NamedDeclaration>, String> {
    let ast_declarations = if let ASTNode::Parenthesis { nodes } = node {
        // Already split (by parenthesis node)
        nodes
    } else {
        // Split here
        separate_commas(node)
    };

    fn deconstruct_ast_declaration(d: ASTNode) -> Result<NamedDeclaration, String> {
        match d {
            ASTNode::ExprIdentifier { name } => {
                Ok(NamedDeclaration {
                    name,
                    typ: None,
                    value: None,
                    conditional: false,
                    spread: false
                })
            }
            ASTNode::Declaration { on, typ, conditional } => {
                let mut decl = deconstruct_ast_declaration(*on)?;
                decl.typ = Some(typ);
                decl.conditional = conditional;
                Ok(decl)
            }
            ASTNode::InfixOpr { left, opr, right } => {
                if opr == "=" {
                    let mut decl = deconstruct_ast_declaration(*left)?;
                    decl.value = Some(*right);
                    Ok(decl)
                } else {
                    return Err(format!(
                        "Expected `=`, found {}",
                        opr
                    ));
                }
            }
            ASTNode::PrefixOpr { opr, expr } => {
                if opr == "..." {
                    let mut decl = deconstruct_ast_declaration(*expr)?;
                    decl.spread = true;
                    Ok(decl)
                } else {
                    return Err(format!("Expected `...`, found {}", opr));
                }
            }
            other => {
                return Err(format!(
                    "Expected declaration, found {:?}",
                    other
                ));
            }
        }
    }

    let mut declarations = Vec::with_capacity(ast_declarations.len());
    for ast_declaration in ast_declarations {
        if !matches!(ast_declaration, ASTNode::Empty) {
            declarations.push(deconstruct_ast_declaration(ast_declaration)?);
        }
    }

    Ok(declarations)
}

/// Handles control flow, like `if`, `while`, and `for`
fn handle_control_flow(
    tokens: &mut TokenList,
    out: &mut Vec<ASTNode>,
) -> Result<bool, String> {
    const CONTROL_FLOW: &[&str] = &[ "if", "while", "for", "switch" ];
    if !CONTROL_FLOW.contains(&tokens.peek_str()) {
        return Ok(false);
    }

    let control_flow_type = tokens.consume().value.to_string();
    tokens.ignore_whitespace();

    out.push(match control_flow_type.as_str() {
        "if" | "while" => {
            // Get condition
            tokens.skip(&[ "(" ])?;
            let condition = get_expression(tokens, 0)?;
            tokens.ignore_whitespace();
            tokens.skip(&[ ")" ])?;

            tokens.ignore_whitespace();
            
            // Get body
            let body = get_single_statement(tokens)?;

            // Add to parent
            if control_flow_type == "if" {
                // Check for `else` block...
                tokens.ignore_whitespace();

                let alternate = if tokens.peek_str() == "else" {
                    tokens.skip_unchecked();
                    Some(Box::new(get_single_statement(tokens)?))
                } else {
                    None
                };
                ASTNode::StatementIf {
                    condition: Box::new(condition),
                    body: Box::new(body),
                    alternate
                }
            } else {
                ASTNode::StatementWhile {
                    condition: Box::new(condition),
                    body: Box::new(body)
                }
            }
        },
        "for" => {
            // TODO: handle `for async`

            // Get header
            tokens.skip(&[ "(" ])?;
            let init = get_single_statement(tokens)?;
            let condition = get_single_statement(tokens)?;
            let update = get_single_statement(tokens)?;
            tokens.skip(&[ ")" ])?;

            // Get body
            let body = get_single_statement(tokens)?;

            ASTNode::StatementFor {
                init: Box::new(init),
                condition: Box::new(condition),
                update: Box::new(update),
                body: Box::new(body)
            }
        },
        // "switch" => {},
        other => { panic!("Control flow not implemented: {}", other); }
    });

    Ok(true)
}

/// Handles `return`, `break`, `continue`, and `throw`
fn handle_other_statements(
    tokens: &mut TokenList,
    out: &mut Vec<ASTNode>,
) -> Result<bool, String> {
    const STATEMENT_NAMES: &[&str] = &[
        "return",
        "break",
        "continue",
        "throw"
    ];
    if !STATEMENT_NAMES.contains(&tokens.peek_str()) {
        return Ok(false);
    }

    let statement_type = tokens.consume().value.to_string();
    tokens.ignore_whitespace();

    let value = if tokens.peek_str() != ";" {
        Some(Box::new(get_expression(tokens, 0)?))
    } else {
        None
    };

    out.push(match statement_type.as_str() {
        "return" => ASTNode::StatementReturn { value },
        "break" => ASTNode::StatementBreak { value },
        "continue" => ASTNode::StatementContinue { value },
        "throw" => ASTNode::StatementThrow { value },
        other => { panic!("Statement not implemented: {}", other); }
    });

    Ok(true)
}

fn handle_function_declaration(
    tokens: &mut TokenList,
    out: &mut Vec<ASTNode>,
) -> Result<bool, String> {
    if tokens.peek_str() != "function" {
        return Ok(false);
    }

    tokens.skip_unchecked(); // Skip `function`

    // Get name
    tokens.ignore_whitespace();
    let name = if matches!(tokens.peek().typ, TokenType::Identifier) {
        // Named function
        Some(tokens.consume().value.to_string())
    } else {
        // Unnamed function
        None
    };
    
    // Get generics
    tokens.ignore_whitespace();
    let mut generics = vec![];
    if tokens.peek_str() == "<" {
        tokens.skip_unchecked();
        loop {
            let new_type = get_type(tokens)?;

            // Skip comma
            tokens.ignore_whitespace();
            while tokens.peek_str() == "," {
                tokens.skip_unchecked();
                tokens.ignore_whitespace();
            }

            let default = if tokens.peek_str() == "=" {
                // Generic default
                tokens.skip_unchecked();
                Some(get_type(tokens)?)
            } else {
                None
            };

            // Add the type
            generics.push((new_type, default));

            if tokens.peek_str() == ">" {
                break;
            }
        }
        tokens.skip_unchecked();
    }
    
    // Get parameters and return type
    let (params, return_type) = {
        tokens.ignore_whitespace();
        let header = get_expression(tokens, 0)?;

        if let ASTNode::Declaration { on, typ, conditional: _ } = header {
            // Parameters with return type
            (
                convert_to_typed_declarations(*on)?,
                Some(typ)
            )
        } else {
            // Only parameters, no return type
            (
                convert_to_typed_declarations(header)?,
                None
            )
        }
    };

    // Get body
    tokens.ignore_whitespace();
    let body = if tokens.peek_str() == ";" {
        // No body! This means it's just a declaration.
        None
    } else {
        tokens.skip(&[ "{" ])?;
        Some(get_block(tokens)?)
    };

    out.push(ASTNode::FunctionDefinition {
        modifiers: Default::default(),
        name,
        generics: if generics.is_empty() { None } else { Some(generics) },
        params,
        return_type,
        body: body.map(Box::new)
    });

    Ok(true)
}

fn handle_modifiers(
    tokens: &mut TokenList,
    out: &mut Vec<ASTNode>,
) -> Result<bool, String> {
    if !MODIFIERS.contains(&tokens.peek_str()) {
        return Ok(false);
    }

    let mut modifiers: ModifierList = Default::default();
    while MODIFIERS.contains(&tokens.peek_str()) {
        match tokens.consume().value {
            "export" => { modifiers.set(Modifier::Export); },
            "async" => { modifiers.set(Modifier::Async); },
            "public" => { modifiers.set(Modifier::Public); },
            "private" => { modifiers.set(Modifier::Private); },
            "protected" => { modifiers.set(Modifier::Protected); },
            "static" => { modifiers.set(Modifier::Static); },
            other => { panic!("Modifier not implemented: {:?}", other); }
        };
        tokens.ignore_whitespace();
    }

    let mut node_after_modifiers = get_single_statement(tokens)?;
    node_after_modifiers.apply_modifiers(modifiers)?;
    out.push(node_after_modifiers);

    Ok(true)
}

fn handle_type_declaration(
    tokens: &mut TokenList,
    out: &mut Vec<ASTNode>,
) -> Result<bool, String> {
    if tokens.peek_str() != "type" {
        return Ok(false);
    }

    tokens.skip_unchecked(); // Skip `type`

    // Get the first type
    let first_typ = get_type(tokens)?;

    // Consume `=`
    tokens.ignore_whitespace();
    tokens.skip(&[ "=" ])?;

    // Get the second type
    let equals_typ = get_type(tokens)?;

    out.push(ASTNode::TypeDeclaration {
        first_typ,
        equals_typ
    });

    Ok(true)
}

/// Handles expressions. Basically a soft wrapper around `get_expression`
fn handle_expression(
    tokens: &mut TokenList,
    out: &mut Vec<ASTNode>,
) -> Result<(), String> {
    out.push(get_expression(tokens, 0)?);
    Ok(())
}

fn parse_number(tokens: &mut TokenList) -> ASTNode {
    ASTNode::ExprNumLiteral {
        number: tokens.consume().value.to_string()
    }
}

fn parse_string(tokens: &mut TokenList) -> ASTNode {
    ASTNode::ExprStrLiteral {
        string: tokens.consume().value.to_string()
    }
}

fn parse_name(tokens: &mut TokenList) -> Result<ASTNode, String> {
    if tokens.peek_str() == "function" {
        // Handle inline functions
        let mut function_container = vec![];
        handle_function_declaration(tokens, &mut function_container)?;
        return Ok(function_container.pop().unwrap());
    }

    match tokens.consume().value {
        "true" => { Ok(ASTNode::ExprBoolLiteral { value: true }) }
        "false" => { Ok(ASTNode::ExprBoolLiteral { value: false }) }
        name => { Ok(ASTNode::ExprIdentifier { name: name.to_string() }) }
    }
}

fn parse_prefix(
    tokens: &mut TokenList,
    precedence: u8
) -> Result<ASTNode, String> {
    let prefix_start = tokens.consume().value.to_string();

    let is_grouping = [ "(", "[" ].contains(&prefix_start.as_str());
    if is_grouping {
        // Groupings get special treatment!
        let nodes = separate_commas(get_expression(tokens, 0)?);
        let ret = match prefix_start.as_str() {
            "(" => ASTNode::Parenthesis { nodes },
            "[" => ASTNode::Array { nodes },
            unknown_grouping => {
                panic!("Grouping not implemented: {}", unknown_grouping);
            }
        };
        tokens.skip(&[ ")", "]" ])?; // Skip grouping close ")" or "]"
        Ok(ret)
    } else if prefix_start == "{" {
        // Dicts get even more special treatment!
        let mut properties = vec![];
        loop {
            tokens.ignore_whitespace();
            let key_token = tokens.peek();
            if key_token.value == "}" { break; }

            if key_token.value == "..." {
                // Spread
                tokens.skip_unchecked(); // Skip "..."

                properties.push(ObjectProperty::Spread {
                    argument: get_expression(tokens, 1)?
                });
            } else {
                // String, identifier, or computed key
                let (computed, key) = match (&key_token.typ, key_token.value) {
                    (TokenType::String, _) => {
                        (false, ASTNode::ExprStrLiteral { string: tokens.consume().value.to_string() })
                    },
                    (TokenType::Identifier, _) => {
                        (false, ASTNode::ExprIdentifier { name: tokens.consume().value.to_string() })
                    }
                    (TokenType::Symbol, "[") => {
                        // Computed key
                        tokens.skip_unchecked();
                        let inner_key = get_expression(tokens, 0)?;
    
                        tokens.ignore_whitespace();
                        tokens.skip(&[ "]" ])?;
    
                        (true, inner_key)
                    },
                    other => {
                        return Err(format!("Expected key, found {:?}", other));
                    }
                };
    
                // Skip ":"
                tokens.ignore_whitespace();
                tokens.skip(&[ ":" ])?;
    
                // Get value
                let value = get_expression(tokens, 1)?;
                
                properties.push(ObjectProperty::Property {
                    computed,
                    key,
                    value
                });
            }

            // Ignore trailing commas (if any)
            tokens.ignore_whitespace();
            while tokens.peek_str() == "," {
                tokens.skip_unchecked();
                tokens.ignore_whitespace();
            }
        }
        tokens.skip_unchecked(); // Skip closing "}"

        Ok(ASTNode::Dict { properties })
    } else {
        Ok(ASTNode::PrefixOpr {
            opr: prefix_start,
            expr: Box::new(get_expression(tokens, precedence)?)
        })
    }
}

/// Parses an infix operator. Ran when the current token is known to be infix.
fn parse_infix(
    left: ASTNode,
    tokens: &mut TokenList,
    precedence: u8
) -> Result<ASTNode, String> {
    let opr = tokens.consume().value.to_string();

    if opr == "=>" {
        // Arrow functions get special treatment!
        let (params, return_type) = match left {
            ASTNode::Parenthesis { .. } => {
                (convert_to_typed_declarations(left)?, None)
            },
            ASTNode::Declaration { on, typ, .. } => {
                (convert_to_typed_declarations(*on)?, Some(typ))
            }
            ASTNode::ExprIdentifier { name } => {
                (vec![ NamedDeclaration {
                    name,
                    typ: None,
                    value: None,
                    conditional: false,
                    spread: false
                } ], None)
            }
            other => {
                return Err(format!(
                    "Arrow function expected parenthesis or identifier, found {:?}",
                    other
                ));
            }
        };

        return Ok(ASTNode::ArrowFunctionDefinition {
            params,
            return_type,
            body: Box::new(
                if tokens.peek_str() == "{" {
                    get_single_statement(tokens)?
                } else {
                    get_expression(tokens, precedence)?
                }
            )
        });
    } else if opr == ":" {
        // `:` here means what's coming up is a type, not an expression
        let typ = get_type(tokens)?;
        tokens.ignore_whitespace();
        return Ok(ASTNode::Declaration {
            on: Box::new(left),
            typ,
            conditional: false
        });
    } else if opr == "?" {
        // Ternary (eg. `condition ? true : false`)
        // Or maybe a conditional type (eg. `let a?: string;`)

        tokens.ignore_whitespace();
        if tokens.peek_str() == ":" {
            // Conditional type!
            tokens.skip_unchecked();

            // Get the type
            let typ = get_type(tokens)?;
            return Ok(ASTNode::Declaration {
                on: Box::new(left),
                typ,
                conditional: true
            })
        }

        // Get the first part, up until the `:`
        let colon_precedence = get_operator_binding_power(ExprType::Infx, ":").unwrap();
        let mut if_true = get_expression(tokens, colon_precedence.1)?;
        while tokens.peek_str() != ":" {
            if tokens.is_done() {
                return Err("Ternary not closed!".to_string());
            }
            if_true = parse_infix(
                if_true,
                tokens,
                colon_precedence.1
            )?;
            tokens.ignore_whitespace();
        }
        tokens.skip_unchecked();
        let if_false = get_expression(tokens, precedence)?;

        return Ok(ASTNode::ExprTernary {
            condition: Box::new(left),
            if_true: Box::new(if_true),
            if_false: Box::new(if_false)
        });
    }

    // Normal infix
    Ok(ASTNode::InfixOpr {
        left: Box::new(left),
        opr,
        right: Box::new(get_expression(tokens, precedence)?)
    })
}

/// Turns a tree containing commas into a vector with the nodes they separated
fn separate_commas(node: ASTNode) -> Vec<ASTNode> {
    if matches!(node, ASTNode::InfixOpr { .. }) {
        let opr = match &node {
            ASTNode::InfixOpr { ref opr, .. } => { opr },
            _ => ""
        };
        if opr == "," {
            let mut nodes = vec![];
            if let ASTNode::InfixOpr { left, opr: _, right } = node {
                nodes.append(&mut separate_commas(*left));
                nodes.append(&mut separate_commas(*right));
            }
            return nodes;
        }
    }

    // Otherwise, return the node, as it's between commas
    vec![ node ]
}

/// Gets a single expression
fn get_expression(
    tokens: &mut TokenList,
    precedence: u8
) -> Result<ASTNode, String> {
    tokens.ignore_whitespace();
    
    // Get left (or sometimes only) side (which can be the prexfix!)
    let mut left = {
        let next = tokens.peek();
        // println!("Handling prefix {:?}", next);

        if [ ")", "]", "}", ",", ";" ].contains(&next.value) {
            // End it here!
            return Ok(ASTNode::Empty);
        }

        match next.typ {
            TokenType::Number => parse_number(tokens),
            TokenType::String => parse_string(tokens),
            TokenType::Symbol | TokenType::Identifier => {
                let binding_power = get_operator_binding_power(
                    ExprType::Prefx,
                    next.value
                );
                if let Some(binding_power) = binding_power {
                    // Some operator!
                    parse_prefix(tokens, binding_power.1)?
                } else if matches!(next.typ, TokenType::Identifier) {
                    // Could be a name...
                    parse_name(tokens)?
                } else {
                    // Not a name & no matching operators.
                    return Err(format!(
                        "Prefix operator not found: {:?}",
                        tokens.peek()
                    ));
                }
            },
            _ => {
                return Err(format!(
                    "Unexpected token when parsing expression: {:?}",
                    next
                ));
            },
        }
    };

    // If there's a next operator, parse it
    loop {
        // Get the next token
        tokens.ignore_whitespace();
        let next = tokens.peek();
        if next.typ == TokenType::EndOfFile { break; }

        // println!("Handling infix/postfix (w/ min precedence {}): {:?}", precedence, next);

        // `as` statements get special treatment
        if next.value == "as" {
            tokens.skip_unchecked();
            let cast_type = get_type(tokens)?;
            left = ASTNode::ExprAs {
                value: Box::new(left),
                cast_type
            };
            continue;
        }

        // Handle postfix

        if let Some(binding_power) = get_operator_binding_power(
            ExprType::Pstfx,
            next.value
        ) {
            if binding_power.0 < precedence {
                break;
            }
            left = ASTNode::PostfixOpr {
                expr: Box::new(left),
                opr: tokens.consume().value.to_string()
            };
            continue;
        }

        // Handle infix

        let binding_power = if let Some(binding_power) = get_operator_binding_power(
            ExprType::Infx,
            next.value
        ) {
            if binding_power.0 < precedence {
                break;
            }
            binding_power
        } else {
            // If there is no infix for this symbol, stop
            break;
        };

        if [ "(", "[" ].contains(&next.value) {
            // Function calls & indexing get special treatment!
            let group_type = tokens.consume().value.to_string();

            // Get children
            let arguments = separate_commas(
                get_expression(tokens, 0)?
            );
            tokens.skip_unchecked();

            left = match group_type.as_str() {
                "(" => ASTNode::ExprFunctionCall { callee: Box::new(left), arguments },
                "[" => ASTNode::ExprIndexing { callee: Box::new(left), property: arguments },
                other => { panic!("Grouping not implemented: {}", other); }
            };
            continue;
        } else {
            // Otherwise, append to the left operator
            left = parse_infix(left, tokens, binding_power.1)?;
        }

    }
    Ok(left)
}

