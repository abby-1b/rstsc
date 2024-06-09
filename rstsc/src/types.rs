use std::{collections::HashSet, fmt::Display};

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

    Void,

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

    /// A typed object (dict)
    Object(Vec<(String, Type)>),

    /// Used for type guards `x is string`
    Guard(String, Box<Type>),

    /// A typed declaration (used inside arrow functions)
    ColonDeclaration {
        name: String,
        typ: Box<Type>,
        conditional: bool
    },

    /// A function (eg. `let a: (x: number) => void = x => x * 2`)
    Function {
        args: Vec<Type>,
        return_type: Box<Type>
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Any => { f.write_str("any") },

            Type::Number => { f.write_str("number") },
            Type::NumberLiteral(number) => {
                f.write_str(&f64::from_bits(*number).to_string())
            },

            Type::String => { f.write_str("string") },
            Type::StringLiteral(string) => {
                f.write_str(string)
            },

            Type::Boolean => { f.write_str("boolean") },
            Type::BooleanLiteral(boolean) => {
                f.write_str(if *boolean {
                    "true"
                } else {
                    "false"
                })
            },

            Type::Unknown => { f.write_str("unknown") },
            Type::Void => { f.write_str("void") },

            Type::Union(types) => {
                f.write_str(
                    &types.iter()
                        .map(|t| t.to_string()).collect::<Vec<String>>()
                        .join(" | ")
                )
            },

            Type::Custom(name) => { f.write_str(&name) },

            Type::WithArgs(typ, args) => {
                f.write_str(&typ.to_string())?;
                f.write_str(
                    &args.iter()
                        .map(|t| t.to_string()).collect::<Vec<String>>()
                        .join(" | ")
                )
            },

            Type::Tuple(inner_types) => {
                f.write_str("[")?;
                f.write_str(
                    &inner_types.iter()
                        .map(|t| t.to_string()).collect::<Vec<String>>()
                        .join(", ")
                )?;
                f.write_str("]")
            },

            Type::Array(typ) => {
                f.write_str(&typ.to_string())?;
                f.write_str("[]")
            },

            Type::Object(kv_pairs) => {
                f.write_str("{")?;
                f.write_str(
                    &kv_pairs.iter()
                        .map(|kv| {
                            kv.0.clone() + " " + &kv.1.to_string()
                        }).collect::<Vec<String>>()
                        .join(", ")
                )?;
                f.write_str("}")
            },

            Type::Guard(name, typ) => {
                f.write_str(name)?;
                f.write_str(" is ")?;
                f.write_str(&typ.to_string())
            },

            Type::ColonDeclaration {
                name,
                typ,
                conditional
            } => {
                f.write_str(name)?;
                if *conditional { f.write_str("?")?; }
                f.write_str(": ")?;
                f.write_str(&typ.to_string())
            },

            Type::Function {
                args,
                return_type
            } => {
                f.write_str("(")?;
                f.write_str(
                    &args.iter()
                        .map(|arg| arg.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )?;
                f.write_str(") => ")?;
                f.write_str(&return_type.to_string())
            }
        }
    }
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
            "boolean" => Type::Boolean,
            "true" => Type::BooleanLiteral(true),
            "false" => Type::BooleanLiteral(false),
            "void" => Type::Void,
            "asserts" => get_type(tokens)?,
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
            } else if start_token.value == "(" {
                // Parenthesized type!
                // This could be an arrow function, too.

                let mut inner_types = vec![];
                loop {
                    tokens.ignore_whitespace();
                    if tokens.peek_str() == "," {
                        tokens.consume();
                    } else if tokens.peek_str() == ")" {
                        tokens.consume();
                        break;
                    }

                    inner_types.push(get_type(tokens)?);
                }

                tokens.ignore_whitespace();
                if tokens.peek_str() == "=>" {
                    // Function
                    tokens.skip_unchecked();
                    let return_type = get_type(tokens)?;
                    Type::Function {
                        args: inner_types,
                        return_type: Box::new(return_type)
                    }
                } else if inner_types.len() != 1 {
                    return Err(format!(
                        "Found multiple types in a single parenthesis! {:?}",
                        inner_types
                    ));
                } else {
                    // Simple parenthesis, remove the type from inside
                    inner_types.pop().unwrap()
                }
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
            ":" => {
                tokens.skip_unchecked();
                tokens.ignore_whitespace();
                let conditional = if tokens.peek_str() == "?" {
                    tokens.skip_unchecked();
                    true
                } else {
                    false
                };
                let typ = get_type(tokens)?;
                start_type = Type::ColonDeclaration {
                    name: start_type.to_string(),
                    typ: Box::new(typ),
                    conditional
                }
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
