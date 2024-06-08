use std::collections::HashSet;

use crate::tokenizer::{TokenList, TokenType};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Type {
    // Primitives
    Any,

    Number,
    /// Although it's stored as a u64, this actually refers to an f64
    NumberLiteral(u64),

    String,
    StringLiteral(String),

    Boolean,
    BooleanLiteral(bool),

    Unknown,

    /// A union (eg. `string | number`)
    Union(Vec<Type>),

    /// Custom types
    Custom(String),

    /// Arguments (eg. `Record<string, number>`)
    WithArgs(Box<Type>, Vec<Type>),

    /// Tuples (eg. `[string, number]`)
    Tuple(Vec<Type>),

    /// Array type (eg. `number[]`)
    Array(Box<Type>),

    Object(Vec<(String, Type)>),

    /// Used for type guards `x is string`
    Guard(String, Box<Type>),

    // TODO: dicts, extends, all of the complex stuff...
}

/// Gets a type, regardless of whether it starts with `:` or not.
pub fn get_type(tokens: &mut TokenList) -> Result<Type, String> {
    tokens.ignore_whitespace();

    // `string`
    // `string[]`
    // `string[] extends Something`
    // `[string, number]`
    // `[string, number][]`
    // `Record<string, number>`
    // `{ a: string, b: number }`

    // Get the base type `string[] extends Something`
    //                    ^^^^^^
    let start_token = tokens.consume();
    let mut start_type = match start_token.typ {
        TokenType::Identifier => match start_token.value {
            "any" => Type::Any,
            "number" => Type::Number,
            "string" => Type::String,
            "unknown" => Type::Unknown,
            "boolean" => Type::Boolean,
            "true" => Type::BooleanLiteral(true),
            "false" => Type::BooleanLiteral(false),
            _ => { Type::Custom(start_token.value.to_string()) }
        },
        TokenType::Number => {
            // Converts string => f64 => u64
            let number: f64 = start_token.value.parse().unwrap();
            Type::NumberLiteral(number.to_bits())
        },
        TokenType::String => Type::StringLiteral(start_token.value.to_string()),
        TokenType::Symbol => {
            if start_token.value == "{" {
                let mut obj_parts: Vec<(String, Type)> = vec![];
                loop {
                    // TODO: Handle `[key: string]: number`

                    // Get property name
                    tokens.ignore_whitespace();
                    let property = tokens.consume().value.to_string();
                    
                    // Get property type (fallback to `any`)
                    tokens.ignore_whitespace();
                    let property_type = if tokens.peek_str() == ":" {
                        tokens.skip_unchecked();
                        get_type(tokens)?
                    } else {
                        Type::Any
                    };

                    // Push
                    obj_parts.push((
                        property,
                        property_type
                    ));

                    // Ignore commas / exit
                    tokens.ignore_whitespace();
                    while tokens.peek_str() == "," {
                        tokens.skip_unchecked();
                        tokens.ignore_whitespace();
                    }

                    if tokens.peek_str() == "}" {
                        tokens.skip_unchecked();
                        break;
                    }
                }

                Type::Object(obj_parts)
            } else if start_token.value == "[" {
                // Tuple
                let mut tuple_parts: Vec<Type> = vec![];
                loop {
                    tuple_parts.push(get_type(tokens)?);

                    // Ignore commas
                    tokens.ignore_whitespace();
                    while tokens.peek_str() == "," {
                        tokens.skip_unchecked();
                        tokens.ignore_whitespace();
                    }

                    // End on brackets
                    if tokens.peek_str() == "]" {
                        tokens.skip_unchecked();
                        break;
                    }
                }
                Type::Tuple(tuple_parts)
            } else {
                // Unexpected token
                return Err(format!(
                    "Unexpected token when fetching type: {:?}",
                    start_token
                ));
            }
        }
        _ => {
            // Unexpected token
            return Err(format!(
                "Unexpected token when fetching type: {:?}",
                start_token
            ));
        }
    };

    loop {
        tokens.ignore_whitespace();
        match tokens.peek_str() {
            "|" => {
                // Union type
                tokens.skip_unchecked();
                let right_type = get_type(tokens)?;
                start_type = combine_types(&start_type, &right_type);
            }
            "<" => {
                // Arguments to the type
                tokens.skip_unchecked();
                let mut args = vec![];

                while tokens.peek_str() != ">" && !tokens.is_done() {
                    args.push(get_type(tokens)?);
                    tokens.ignore_whitespace();
                    if tokens.peek_str() == "," {
                        tokens.skip_unchecked();
                    }
                    tokens.ignore_whitespace();
                }
                tokens.skip(&[ ">" ])?;

                start_type = Type::WithArgs(Box::new(start_type), args);
            }
            "[" => {
                // Array type
                tokens.skip_unchecked();
                tokens.ignore_whitespace();
                tokens.skip(&[ "]" ])?;

                start_type = Type::Array(Box::new(start_type));
            }
            "is" => {
                // Type guard
                tokens.skip_unchecked();
                tokens.ignore_whitespace();
                match start_type {
                    Type::Custom(var_name) => {
                        start_type = Type::Guard(
                            var_name,
                            Box::new(get_type(tokens)?)
                        )
                    }
                    other => {
                        return Err(format!(
                            "Type guard expected Type::Custom(...), found {:?}",
                            other
                        ))
                    }
                };
            }
            _ => { break; }
        }
    }

    Ok(start_type)
}

/// Combines two types.
/// If the types are equal, the same type is returned.
/// Otherwise, a union is returned.
pub fn combine_types(a: &Type, b: &Type) -> Type {
    if *a == *b {
        // Equal types
        a.clone()
    } else {
        // Different types
        let mut union_set: HashSet<Type> = HashSet::new();
        add_union_types_into_set(a, &mut union_set);
        add_union_types_into_set(b, &mut union_set);

        Type::Union(union_set.into_iter().collect())
    }
}

fn add_union_types_into_set(t: &Type, set: &mut HashSet<Type>) {
    match t {
        Type::Union(types) => set.extend(types.iter().cloned()),
        _ => { set.insert(t.clone()); }
    }
}
