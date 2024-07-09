use std::collections::hash_map::DefaultHasher;
use std::fmt::{Display, Debug};
use std::hash::{Hash, Hasher};

use crate::operations::{get_type_operator_binding_power, ExprType};
use crate::parser::INVERSE_GROUPINGS;
use crate::tokenizer::{TokenList, TokenType};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct TypedNamedDeclaration {
    pub name: String,
    pub typ: Type,
}

#[derive(Copy, Clone)]
pub union CustomDouble {
    value: f64,
    bits: u64
}

impl Display for CustomDouble {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", unsafe { self.value })
    }
}
impl Debug for CustomDouble {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", unsafe { self.value })
    }
}
impl PartialEq for CustomDouble {
    fn eq(&self, other: &Self) -> bool {
        unsafe { self.value == other.value }
    }
}
impl Eq for CustomDouble {}
impl Hash for CustomDouble {
    fn hash<H: Hasher>(&self, state: &mut H) {
        unsafe { self.bits }.hash(state);
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Type {
    // Primitives
    Any,

    Number,
    NumberLiteral(CustomDouble),

    String,
    StringLiteral(String),

    Boolean,
    BooleanLiteral(bool),

    Unknown,

    Void,

    /// A union (eg. `string | number`)
    Union(Vec<Type>),
    
    /// A selection (eg. `string & number`)
    Intersection(Vec<Type>),

    /// Custom types
    Custom(String),

    /// Arguments (eg. `Record<string, number>`)
    WithArgs(Box<Type>, Vec<Type>),

    /// Tuples (eg. `[string, number]`)
    Tuple(Vec<Type>, usize),

    /// Array type (eg. `number[]`)
    Array(Box<Type>),

    /// A typed object (dict)
    Object(Vec<TypedNamedDeclaration>),

    /// Used for type guards `x is string`
    Guard(String, Box<Type>),

    /// A typed declaration (used inside arrow functions)
    ColonDeclaration {
        name: String,
        typ: Box<Type>,
        conditional: bool
    },

    /// A function (eg. `let a: (x: number) => void;`)
    Function {
        args: Vec<Type>,
        return_type: Box<Type>
    },

    /// An index into a type (eg. `type[prop]`)
    Index {
        callee: Box<Type>,
        property: Box<Type>
    },

    KeyOf {
        callee: Box<Type>,
    },

    Conditional {
        cnd_left: Box<Type>,
        cnd_right: Box<Type>,
        if_true: Box<Type>,
        if_false: Box<Type>
    },
}

impl Type {
    /// Joins this type with another in a union `|` (combining mutably)
    pub fn union(&mut self, other: Type) {
        match other {
            Type::Union(types) => {
                // Split other type into its parts
                for typ in types {
                    self.union(typ);
                }
                return;
            },
            _ => {}
        }

        match self {
            Type::Union(types) => {
                let mut hasher = DefaultHasher::new();

                // Get this type's hash
                other.hash(&mut hasher);
                let new_hash = hasher.finish();

                let mut found_type = false;
                for t in types.into_iter() {
                    t.hash(&mut hasher);
                    if hasher.finish() == new_hash {
                        found_type = true;
                        break;
                    }
                }

                // If the hash wasn't found anywhere, add it to the vector
                if !found_type {
                    types.push(other);
                }
            }
            _ => {
                if other != *self {
                    let mut new_union = Type::Union(vec![ self.clone() ]);
                    new_union.union(other);
                    *self = new_union;
                }
            }
        }
    }

    /// Joins this type with another in an intersection `&` (combining mutably)
    pub fn intersection(&mut self, other: Type) {
        match other {
            Type::Intersection(types) => {
                // Split other type into its parts
                for typ in types {
                    self.intersection(typ);
                }
                return;
            },
            _ => {}
        }

        match self {
            Type::Intersection(types) => {
                let mut hasher = DefaultHasher::new();

                // Get this type's hash
                other.hash(&mut hasher);
                let new_hash = hasher.finish();

                let mut found_type = false;
                for t in types.into_iter() {
                    t.hash(&mut hasher);
                    if hasher.finish() == new_hash {
                        found_type = true;
                        break;
                    }
                }

                // If the hash wasn't found anywhere, add it to the vector
                if !found_type {
                    types.push(other);
                }
            }
            _ => {
                if other != *self {
                    let mut new_union = Type::Intersection(vec![ self.clone() ]);
                    new_union.intersection(other);
                    *self = new_union;
                }
            }
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Any => { f.write_str("any") },

            Type::Number => { f.write_str("number") },
            Type::NumberLiteral(number) => {
                f.write_str(&unsafe { number.value }.to_string())
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
            Type::Intersection(types) => {
                f.write_str(
                    &types.iter()
                        .map(|t| t.to_string()).collect::<Vec<String>>()
                        .join(" & ")
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

            Type::Tuple(inner_types, spread_idx) => {
                let spread_idx = *spread_idx;
                f.write_str("[")?;
                f.write_str(
                    &inner_types.iter().enumerate()
                        .map(|(index, typ)| {
                            if spread_idx == index {
                                "...".to_owned() + &typ.to_string()
                            } else {
                                typ.to_string()
                            }
                        }).collect::<Vec<String>>()
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
                            kv.name.clone() + " " + &kv.typ.to_string()
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
            Type::Index { callee, property } => {
                f.write_str(&callee.to_string())?;
                f.write_str("[")?;
                f.write_str(&property.to_string())?;
                f.write_str("]")
            }
            Type::KeyOf { callee } => {
                f.write_str("keyof ")?;
                f.write_str(&callee.to_string())
            }
            Type::Conditional {
                cnd_left, cnd_right,
                if_true, if_false
            } => {
                f.write_str(&cnd_left.to_string())?;
                f.write_str(" extends ")?;
                f.write_str(&cnd_right.to_string())?;
                f.write_str(" ? ")?;
                f.write_str(&if_true.to_string())?;
                f.write_str(" : ")?;
                f.write_str(&if_false.to_string())
            }
        }
    }
}

/// Gets a type, regardless of whether it starts with `:` or not.
pub fn get_type(tokens: &mut TokenList) -> Result<Type, String> {
    if tokens.peek_str() == ":" {
        tokens.skip_unchecked();
    }

    return get_expression(tokens, 0);
}

fn parse_infix(
    mut left: Type,
    tokens: &mut TokenList,
    precedence: u8
) -> Result<Type, String> {
    let infix_opr = tokens.consume();
    match infix_opr.value {
        "|" => {
            left.union(get_expression(tokens, precedence)?);
            Ok(left)
        }
        "&" => {
            left.intersection(get_expression(tokens, precedence)?);
            Ok(left)
        }
        "is" => {
            // Type guard
            tokens.skip_unchecked();
            tokens.ignore_whitespace();
            match left {
                Type::Custom(var_name) => {
                    Ok(Type::Guard(
                        var_name,
                        Box::new(get_expression(tokens, precedence)?)
                    ))
                }
                other => {
                    return Err(format!(
                        "Type guard expected Type::Custom(...), found {:?}",
                        other
                    ))
                }
            }
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
            let typ = get_expression(tokens, precedence)?;
            Ok(Type::ColonDeclaration {
                name: left.to_string(),
                typ: Box::new(typ),
                conditional
            })
        }
        "[" | "<" => {
            // Type indexing and generics
            let group_start = infix_opr.value.to_string();
            let group_end = INVERSE_GROUPINGS[&group_start];

            let mut arguments: Vec<Type> = vec![];
            tokens.ignore_whitespace();
            if tokens.peek_str() != group_end {
                loop {
                    tokens.ignore_whitespace();
                    arguments.push(get_expression(tokens, 0)?);
                    if tokens.peek_str() != "," {
                        break;
                    }
                    tokens.skip_unchecked(); // Skip comma
                }
            }

            tokens.skip(&[ &group_end ])?;

            if group_start == "[" {
                if arguments.len() == 0 {
                    // Normal array notation
                    Ok(Type::Array(Box::new(left)))
                } else if arguments.len() == 1 {
                    Ok(Type::Index {
                        callee: Box::new(left),
                        property: Box::new(arguments.pop().unwrap())
                    })
                } else {
                    Err(format!(
                        "Type indices need exactly one argument, found {}",
                        arguments.len()
                    ))
                }
            } else {
                // Generic arguments
                Ok(Type::WithArgs(Box::new(left), arguments))
            }
        }
        other => {
            Err(format!(
                "Unexpected infix in type '{:?}'",
                other
            ))
        }
    }
}

fn parse_name(
    tokens: &mut TokenList
) -> Type {
    match tokens.consume().value {
        "any" => Type::Any,
        "number" => Type::Number,
        "string" => Type::String,
        "boolean" => Type::Boolean,
        "true" => Type::BooleanLiteral(true),
        "false" => Type::BooleanLiteral(false),
        "void" => Type::Void,
        other => { Type::Custom(other.to_string()) }
    }
}

fn parse_prefix(
    tokens: &mut TokenList,
    precedence: u8
) -> Result<Type, String> {
    match tokens.consume().value {
        "{" => {
            // Dictionary object
            let mut obj_parts: Vec<TypedNamedDeclaration> = vec![];
            loop {
                // TODO: Handle `[key: string]: number`

                tokens.ignore_whitespace();
                if tokens.peek_str() == "}" {
                    tokens.skip_unchecked();
                    break;
                }

                // Get property name
                let property = tokens.consume().value.to_string();
                
                // Get property type (fallback to `any`)
                tokens.ignore_whitespace();
                let property_type = if tokens.peek_str() == ":" {
                    tokens.skip_unchecked();
                    get_expression(tokens, precedence)?
                } else {
                    Type::Any
                };

                // Push
                obj_parts.push(TypedNamedDeclaration {
                    name: property,
                    typ: property_type
                });

                // Ignore commas / exit
                tokens.ignore_whitespace();
                while tokens.peek_str() == "," {
                    tokens.skip_unchecked();
                }
            }

            Ok(Type::Object(obj_parts))
        }
        "[" => {
            // Tuple
            let mut tuple_parts: Vec<Type> = vec![];
            let mut spread_idx = usize::MAX;
            loop {
                if tokens.peek_str() == "..." {
                    // Get spread
                    if spread_idx != usize::MAX {
                        // Multiple spreads are not allowed
                        // eg. `let a: [boolean, ...number[], ...string[]]`
                        return Err(
                            "Can't have multiple spread elements in one tuple.".to_string()
                        );
                    }
                    spread_idx = tuple_parts.len();
                    tokens.skip_unchecked(); // Skip "..."
                }
                tuple_parts.push(get_expression(tokens, precedence)?);

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
            Ok(Type::Tuple(tuple_parts, spread_idx))
        }
        "(" => {
            // Parenthesized type!
            // This could be an arrow function, too

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
            if tokens.peek_str() == "=>" || tokens.peek_str() == ":" {
                // Function
                tokens.skip_unchecked();
                let return_type = get_expression(tokens, precedence)?;
                Ok(Type::Function {
                    args: inner_types,
                    return_type: Box::new(return_type)
                })
            } else if inner_types.len() != 1 {
                Ok(Type::Function {
                    args: inner_types,
                    return_type: Box::new(Type::Unknown)
                })
            } else {
                // Simple parenthesis, remove the type from inside
                Ok(inner_types.pop().unwrap())
            }
        }
        "asserts" => {
            get_expression(tokens, precedence)
        }
        "keyof" => {
            Ok(Type::KeyOf {
                callee: Box::new(get_expression(tokens, precedence)?)
            })
        }
        other => {
            Err(format!(
                "Prefix operator not found: {:?}",
                other
            ))
        }
    }
}

fn get_expression(
    tokens: &mut TokenList,
    precedence: u8
) -> Result<Type, String> {
    tokens.ignore_whitespace();

    let mut left = {
        let next = tokens.peek();
        // println!("Handling prefix {:?}", next);

        if [ ")", "]", "}", ",", ";" ].contains(&next.value) {
            // End it here!
            return Ok(Type::Unknown);
        }

        let next = tokens.peek();
        match &next.typ {
            TokenType::Number => {
                let value: f64 = tokens.consume().value.parse().unwrap();
                Type::NumberLiteral(CustomDouble {
                    value
                })
            }
            TokenType::String => {
                Type::StringLiteral(tokens.consume().value.to_string())
            }
            TokenType::Identifier | TokenType::Symbol => {
                let binding_power = get_type_operator_binding_power(
                    ExprType::Prefx,
                    next.value
                );
                if let Some(binding_power) = binding_power {
                    // Prefix operators
                    parse_prefix(tokens, binding_power.1)?
                } else if matches!(next.typ, TokenType::Identifier) {
                    // Could be a name...
                    parse_name(tokens)
                } else {
                    return Err(format!(
                        "Prefix operator not found when fetching type: {:?}",
                        next
                    ))
                }
            }
            other => {
                return Err(format!(
                    "Unexpected token when parsing type: {:?}",
                    other
                ));
            }
        }
    };

    loop {
        // Get the next token
        tokens.ignore_whitespace();
        let next = tokens.peek();
        if next.typ == TokenType::EndOfFile { break; }

        // println!(
        //     "Handling infix/postfix (w/ min precedence {}) in type: {:?}",
        //     precedence, next
        // );

        // Handle infix
        let binding_power = if let Some(binding_power) = get_type_operator_binding_power(
            ExprType::Infx, next.value
        ) {
            if binding_power.0 < precedence {
                break;
            }
            binding_power
        } else {
            // No infix for this symbol!
            break;
        };

        left = parse_infix(left, tokens, binding_power.1)?;
    }

    Ok(left)
}