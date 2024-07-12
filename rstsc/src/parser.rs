use phf::phf_map;
use crate::{
    ast::{
        ASTNode, ClassDefinition, FunctionDefinition, Modifier, ModifierList, NamedDeclaration, ObjectProperty, VariableDefType, MODIFIERS
    },
    error_type::CompilerError,
    operations::{get_operator_binding_power, ExprType},
    tokenizer::{TokenList, TokenType, EOF_TOKEN},
    types::{get_key_value, get_type, Type, TypedNamedDeclaration}
};

pub static INVERSE_GROUPINGS: phf::Map<&'static str, &'static str> = phf_map! {
    "(" => ")",
    "[" => "]",
    "{" => "}",
    "<" => ">",
    
    ")" => "(",
    "]" => "[",
    "}" => "{",
    ">" => "<",
};

pub static ANONYMOUS_CLASS_NAME: &'static str = "\0";

/// Parses a single block. Consumes the ending token!
pub fn get_block<'a>(tokens: &mut TokenList<'a>) -> Result<ASTNode, CompilerError<'a>> {
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
fn get_single_statement<'a, 'b>(
    tokens: &'b mut TokenList<'a>
) -> Result<ASTNode, CompilerError<'a>> where 'a: 'b {
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
            handle_class_declaration(tokens, &mut nodes)? ||
            handle_modifiers(tokens, &mut nodes)? ||
            handle_other_statements(tokens, &mut nodes)? ||
            handle_type_declaration(tokens, &mut nodes)? ||
            handle_interface(tokens, &mut nodes)?
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
fn handle_blocks<'a>(
    tokens: &mut TokenList<'a>,
    out: &mut Vec<ASTNode>,
) -> Result<bool, CompilerError<'a>> {
    if tokens.peek_str() != "{" {
        return Ok(false);
    }

    tokens.skip_unchecked();
    out.push(get_block(tokens)?);

    Ok(true)
}

/// Handles variable initialization
fn handle_vars<'a, 'b>(
    tokens: &'b mut TokenList<'a>,
    out: &mut Vec<ASTNode>,
) -> Result<bool, CompilerError<'a>> where 'a: 'b {
    const VARIABLE_DECLARATIONS: &[&str] = &[ "var", "let", "const" ];
    if !VARIABLE_DECLARATIONS.contains(&tokens.peek_str()) {
        return Ok(false);
    }

    let def_type = get_variable_def_type(tokens)?;

    // Get the variable definitions
    let defs = get_named_declarations(tokens)?;

    out.push(ASTNode::VariableDeclaration {
        modifiers: Default::default(),
        def_type,
        defs
    });

    Ok(true)
}

fn get_variable_def_type<'b>(
    tokens: &mut TokenList<'b>
) -> Result<VariableDefType, CompilerError<'b>> {
    // Get the header
    let header_token = tokens.consume();
    match header_token.value {
        "var"   => Ok(VariableDefType::Var),
        "let"   => Ok(VariableDefType::Let),
        "const" => Ok(VariableDefType::Const),
        other => {
            return Err(CompilerError {
                message: format!("Unexpected var declaration: {}", other),
                token: header_token
            })
        }
    }
}

/// Gets typed declarations, with or without values. Stops when it encounters a semicolon.
/// 
/// Examples (in square brackets):
/// 
/// `let [a: number = 123, b: string];`
/// 
/// `function some([a: number, b: string = '123']) { ... }`
fn get_named_declarations<'a, 'b>(
    tokens: &'b mut TokenList<'a>
) -> Result<Vec<NamedDeclaration>, CompilerError<'a>> where 'a: 'b {
    let declaration_node = get_expression(tokens, 0)?;
    convert_to_typed_declarations(declaration_node)
}

fn convert_to_typed_declarations(
    node: ASTNode,
) -> Result<Vec<NamedDeclaration>, CompilerError<'static>> {
    let ast_declarations = if let ASTNode::Parenthesis { nodes } = node {
        // Already split (by parenthesis node)
        nodes
    } else {
        // Split here
        separate_commas(node)
    };

    fn deconstruct_ast_declaration(d: ASTNode) -> Result<NamedDeclaration, CompilerError<'static>> {
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
                    Err(CompilerError {
                        message: format!(
                            "Expected `=`, found {}",
                            opr
                        ),
                        token: EOF_TOKEN.clone()
                    })
                }
            }
            ASTNode::PrefixOpr { opr, expr } => {
                if opr == "..." {
                    let mut decl = deconstruct_ast_declaration(*expr)?;
                    decl.spread = true;
                    Ok(decl)
                } else {
                    Err(CompilerError {
                        message: format!("Expected `...`, found {}", opr),
                        token: EOF_TOKEN.clone()
                    })
                }
            }
            other => {
                Err(CompilerError {
                    message: format!(
                        "Expected declaration, found {:?}",
                        other
                    ),
                    token: EOF_TOKEN.clone()
                })
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
fn handle_control_flow<'a>(
    tokens: &mut TokenList<'a>,
    out: &mut Vec<ASTNode>,
) -> Result<bool, CompilerError<'a>> {
    const CONTROL_FLOW: &[&str] = &[ "if", "while", "for", "switch" ];
    if !CONTROL_FLOW.contains(&tokens.peek_str()) {
        return Ok(false);
    }

    let control_flow_type = tokens.consume().value.to_string();
    tokens.ignore_whitespace();

    out.push(match control_flow_type.as_str() {
        "if" | "while" => {
            // Get condition
            tokens.skip("(")?;
            let condition = get_expression(tokens, 0)?;
            tokens.ignore_whitespace();
            tokens.skip(")")?;

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
            tokens.skip("(")?;
            let init = get_single_statement(tokens)?;
            let condition = get_single_statement(tokens)?;
            let update = get_single_statement(tokens)?;
            tokens.skip(")")?;

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
fn handle_other_statements<'a>(
    tokens: &mut TokenList<'a>,
    out: &mut Vec<ASTNode>,
) -> Result<bool, CompilerError<'a>> {
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

fn handle_function_declaration<'a, 'b>(
    tokens: &'b mut TokenList<'a>,
    out: &mut Vec<ASTNode>,
) -> Result<bool, CompilerError<'a>> where 'a: 'b {
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

    out.push(ASTNode::FunctionDefinition {
        inner: Box::new(get_function_after_name(
            tokens,
            name
        )?)
    });

    Ok(true)
}

fn get_function_after_name<'a, 'b>(
    tokens: &'b mut TokenList<'a>,
    name: Option<String>
) -> Result<FunctionDefinition, CompilerError<'a>> where 'a: 'b {
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

            if tokens.peek_str() == "=" {
                // Generic default
                // Defaults are ignored! They're an artifact of TypeScript's
                // older type inference, which couldn't infer a lot of the more
                // complex types.
                tokens.skip_unchecked();
                get_type(tokens)?;
            }

            // Add the type
            generics.push(new_type);

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
        tokens.skip("{")?;
        Some(get_block(tokens)?)
    };

    Ok(FunctionDefinition {
        modifiers: Default::default(),
        name,
        generics,
        params,
        return_type,
        body: body.map(Box::new)
    })
}

fn handle_class_declaration<'a, 'b>(
    tokens: &'b mut TokenList<'a>,
    out: &mut Vec<ASTNode>,
) -> Result<bool, CompilerError<'a>> where 'a: 'b {
    if tokens.peek_str() != "class" {
        return Ok(false);
    }
    tokens.skip_unchecked(); // Skip "class"
    tokens.ignore_whitespace();

    fn deconstruct_class_header<'a>(
        header_type: Type
    ) -> Result<(Option<String>, Option<Vec<Type>>, Option<Type>), CompilerError<'a>> {
        match header_type {
            Type::Custom(name) => {
                Ok((Some(name), None, None))
            }
            Type::WithArgs(name_type, generics) => {
                let mut out = deconstruct_class_header(*name_type)?;
                out.1 = Some(generics);
                Ok(out)
            }
            Type::Extends(left, extends) => {
                let mut out = deconstruct_class_header(*left)?;
                out.2 = Some(*extends);
                Ok(out)
            }
            other => {
                Err(CompilerError {
                    message: format!(
                        "Expected class header, found {:?}",
                        other
                    ),
                    token: EOF_TOKEN.clone()
                })
            }
        }
    }

    // Get the class name and generics

    // TODO: fix anonymous class generics `let a = class<T> {...}`
    let (name, generics, extends) = deconstruct_class_header(get_type(tokens)?)?;
    let name: String = name.unwrap_or(ANONYMOUS_CLASS_NAME.to_string());
    let generics = generics.unwrap_or(vec![]);

    // Classes change the way things are parsed!
    let mut declarations = vec![];
    let mut methods = vec![];

    tokens.ignore_whitespace();
    tokens.skip("{")?; // Skip body "{"
    tokens.ignore_whitespace();

    while tokens.peek_str() != "}" {
        // Note: here, "property" refers to anything defined with "=", which is
        // not inherently a function. "method" refers to things defined with
        // parenthesis, making them *always* be a function (or its declaration)

        // Get modifiers (if any)
        let modifiers = fetch_modifier_list(tokens);

        let checkpoint = tokens.get_checkpoint();

        // Get the name (might be a property or a method, we don't know yet)
        let name = tokens.consume();
        if !matches!(name.typ, TokenType::Identifier) {
            dbg!(declarations);
            dbg!(methods);
            return Err(CompilerError {
                message: "Expected identifier in class body!".to_string(),
                token: name
            })
        }
        tokens.ignore_whitespace();

        if [ ":", "=", ";" ].contains(&tokens.peek_str()) {
            // `:`, `=`, and `;` mean it's a property
            tokens.restore_checkpoint(checkpoint);

            // Get the declaration
            let gotten_declarations = get_named_declarations(tokens)?;

            // Add the declaration
            for declaration in gotten_declarations {
                declarations.push((
                    modifiers.clone(),
                    declaration
                ));
            }
        } else {
            // Otherwise, it's a method
            let mut function = get_function_after_name(
                tokens,
                Some(name.value.to_string())
            )?;
            if !function.modifiers.has(Modifier::Public) &&
                !function.modifiers.has(Modifier::Private) &&
                !function.modifiers.has(Modifier::Protected) {
                function.modifiers.set(Modifier::Public);
            }
            methods.push(function);
        }

        tokens.ignore_whitespace();

        // Skip ";" (if any)
        if tokens.peek_str() == ";" {
            tokens.skip_unchecked();
        }

        tokens.ignore_whitespace();
    }
    tokens.skip("}")?; // Skip body "}"

    out.push(ASTNode::ClassDefinition { inner: Box::new(ClassDefinition {
        modifiers: Default::default(),
        name,
        generics,
        extends,
        declarations,
        methods
    }) });

    Ok(true)
}

fn handle_modifiers<'a>(
    tokens: &mut TokenList<'a>,
    out: &mut Vec<ASTNode>,
) -> Result<bool, CompilerError<'a>> {
    if !MODIFIERS.contains(&tokens.peek_str()) {
        return Ok(false);
    }

    // Get the modifiers
    let modifiers = fetch_modifier_list(tokens);

    // Get the node that goes after the modifiers
    let mut node_after_modifiers = get_single_statement(tokens)?;

    node_after_modifiers.apply_modifiers(modifiers)?;
    out.push(node_after_modifiers);

    Ok(true)
}

fn fetch_modifier_list(tokens: &mut TokenList) -> ModifierList {
    let mut modifiers: ModifierList = Default::default();
    while MODIFIERS.contains(&tokens.peek_str()) {
        match tokens.consume().value {
            "export" => { modifiers.set(Modifier::Export); },
            "async" => { modifiers.set(Modifier::Async); },
            "static" => { modifiers.set(Modifier::Static); },
            "public" => { modifiers.set(Modifier::Public); },
            "private" => { modifiers.set(Modifier::Private); },
            "protected" => { modifiers.set(Modifier::Protected); },
            "readonly" => { modifiers.set(Modifier::Readonly); },
            "abstract" => { modifiers.set(Modifier::Abstract); },
            other => { panic!("Modifier not implemented: {:?}", other); }
        };
        tokens.ignore_whitespace();
    }
    modifiers
}

fn handle_type_declaration<'a>(
    tokens: &mut TokenList<'a>,
    out: &mut Vec<ASTNode>,
) -> Result<bool, CompilerError<'a>> {
    if tokens.peek_str() != "type" {
        return Ok(false);
    }

    tokens.skip_unchecked(); // Skip `type`

    // Get the first type
    let first_typ = get_type(tokens)?;

    // Consume `=`
    tokens.ignore_whitespace();
    tokens.skip("=")?;

    // Get the second type
    let equals_typ = get_type(tokens)?;

    out.push(ASTNode::TypeDeclaration {
        first_typ,
        equals_typ
    });

    Ok(true)
}

fn handle_interface<'a>(
    tokens: &mut TokenList<'a>,
    out: &mut Vec<ASTNode>
) -> Result<bool, CompilerError<'a>> {
    if tokens.peek_str() != "interface" {
        return Ok(false);
    }

    tokens.skip_unchecked();

    // Get the interface name
    let name = get_type(tokens)?;
    tokens.ignore_whitespace();

    let extends = if tokens.peek_str() == "extends" {
        tokens.skip_unchecked(); // Skip "extends"
        Some(get_type(tokens)?)
    } else {
        None
    };

    tokens.skip("{")?;

    // TODO: IMPORTANT:
    // Just uhh... get the interface as a type. Types can do everything that
    // interfaces can do. It's pointless.

    // Store the parts of the interface!
    let mut function_types = Type::Intersection(vec![]);
    let mut interface_named_parts: Vec<TypedNamedDeclaration> = vec![];
    let mut key_value = None;

    loop {
        tokens.ignore_whitespace();
        let next = tokens.peek_str();
        if next == "}" {
            tokens.skip_unchecked();
            break;
        }

        if next == "(" {
            // Function
            let function = get_type(tokens)?;
            interface_type.intersection(function);
        } else if next == "[" {
            key_value = Some(get_key_value(tokens)?);
        } else {
            // Normal named declaration (no type)
            let named_decl = get_type(tokens)?;
            match named_decl {
                Type::ColonDeclaration { name, typ, conditional } => {
                    interface_named_parts.push(TypedNamedDeclaration {
                        name,
                        typ: *typ
                    })
                }
                Type::Custom(name) => {
                    interface_named_parts.push(TypedNamedDeclaration {
                        name,
                        typ: Type::Unknown,
                    })
                },
                other => {
                    return Err(CompilerError {
                        message: format!(
                            "Expected type declaration, found {:?}",
                            other
                        ),
                        token: EOF_TOKEN.clone()
                    });
                }
            }
        }

        tokens.ignore_whitespace();
        while [ ";", "," ].contains(&tokens.peek_str()) {
            tokens.skip_unchecked();
            tokens.ignore_whitespace();
        }
    }

    // dbg!(interface_type);
    // dbg!(interface_named_parts);
    // TODO: finish interface typing
    // interface_type.combine(named_declarations_to_type(interface_named_parts)?);

    out.push(ASTNode::TypeDeclaration {
        first_typ: name,
        equals_typ: Type::Any
    });

    Ok(true)
}

/// Handles expressions. Basically a soft wrapper around `get_expression`
fn handle_expression<'a>(
    tokens: &mut TokenList<'a>,
    out: &mut Vec<ASTNode>,
) -> Result<(), CompilerError<'a>> {
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

fn parse_name<'a>(
    tokens: &mut TokenList<'a>
) -> Result<ASTNode, CompilerError<'a>> {
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

fn parse_prefix<'a>(
    tokens: &mut TokenList<'a>,
    precedence: u8
) -> Result<ASTNode, CompilerError<'a>> {
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
        let group_end = INVERSE_GROUPINGS[prefix_start.as_str()];
        tokens.skip(group_end)?; // Skip grouping close ")" or "]"
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
                        tokens.skip("]")?;
    
                        (true, inner_key)
                    },
                    other => {
                        return Err(CompilerError {
                            message: format!("Expected key, found {:?}", other),
                            token: key_token.clone()
                        });
                    }
                };
    
                // Skip ":"
                tokens.ignore_whitespace();
                tokens.skip(":")?;
    
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
fn parse_infix<'a>(
    left: ASTNode,
    tokens: &mut TokenList<'a>,
    precedence: u8
) -> Result<ASTNode, CompilerError<'a>> {
    let opr = tokens.consume().value.to_string();

    if opr == "(" || opr == "[" {
        // Function call or property access (indexing)

        // Get children
        let inner = get_expression(tokens, 0)?;
        let group_end = INVERSE_GROUPINGS[opr.as_str()];
        tokens.skip(group_end)?; // Skip trailing group

        match opr.as_str() {
            "(" => return Ok(ASTNode::ExprFunctionCall {
                callee: Box::new(left),
                arguments: separate_commas(inner)
            }),
            "[" => return Ok(ASTNode::ExprIndexing {
                callee: Box::new(left),
                property: Box::new(inner)
            }),
            other => { panic!("Grouping not implemented: {}", other); }
        };
    } else if opr == "=>" {
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
                return Err(CompilerError {
                    message: format!(
                        "Arrow function expected parenthesis or identifier, found {:?}",
                        other
                    ),
                    token: EOF_TOKEN.clone()
                })
            }
        };

        tokens.ignore_whitespace();
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
                return Err(CompilerError {
                    message: "Ternary not closed!".to_string(),
                    token: tokens.consume()
                });
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
fn get_expression<'a>(
    tokens: &mut TokenList<'a>,
    precedence: u8
) -> Result<ASTNode, CompilerError<'a>> {
    tokens.ignore_whitespace();
    
    // Get left (or sometimes only) side (which can be the prexfix!)
    let mut left = {
        let next = tokens.peek();

        if [ ")", "]", "}", ",", ";" ].contains(&next.value) {
            // End it here!
            return Ok(ASTNode::Empty);
        }

        match next.typ {
            TokenType::Number => parse_number(tokens),
            TokenType::String => parse_string(tokens),
            TokenType::Symbol | TokenType::Identifier => {
                // `class` can be used inside an expression, but calling it a
                // prefix feels strange... I'm going to handle it here
                // TODO: fix class expressions
                // if next.value == "class" {
                //     handle_class_declaration(tokens, out)
                // }

                let binding_power = get_operator_binding_power(
                    ExprType::Prefx,
                    next.value
                );
                if let Some(binding_power) = binding_power {
                    // These prefix operators include what you might expect
                    // (+, -, ~), along with arrays, dicts, among other things!
                    parse_prefix(tokens, binding_power.1)?
                } else if matches!(next.typ, TokenType::Identifier) {
                    // Could be a name...
                    parse_name(tokens)?
                } else {
                    // Not a name & no matching operators.
                    return Err(CompilerError {
                        message: format!(
                            "Prefix operator not found: {:?}",
                            tokens.peek()
                        ),
                        token: tokens.consume()
                    });
                }
            },
            _ => {
                return Err(CompilerError {
                    message: format!(
                        "Unexpected token when parsing expression: {:?}",
                        next
                    ),
                    token: tokens.consume()
                });
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

        // It's infix
        left = parse_infix(left, tokens, binding_power.1)?;
    }
    Ok(left)
}

