use crate::tokenizer::{Token, TokenType};

const VARIABLE_DEFINITION: [&str; 3] = ["const", "let", "var"];
const REMOVE_KEYWORDS: [&str; 4] = [
	"private",
	"public",
	"abstract",
	"protected"
];
const CLOSING_BRACKETS: &'static str = ")]}";

#[derive(Debug, Clone)]
enum FrameType {
	TopLevel,
	Block,

	/// Functions ///
	FunctionName,
	FunctionParams,
	FunctionOptionalReturnType,
	FunctionBody,

	/// Variables ///
	VariableName,
	VariableAfterName,

	/// Types ///
	Type,
	TypeAfter,
	TypeDict,
	TypeTupleOrIndex,
	TypeInfoEndOrSeparator,
	TypeIndexSignature,

	/// Interfaces ///
	InterfaceHeader,
	InterfaceBody,

	/// Enums ///
	EnumHeader { name: Option<String> },
	EnumBody {
		name: String,
		entries: Vec<(String, Vec<Token>)>,
		curr_name: String,
	},

	/// Something that is immediately ignored (and deleted) ///
	IgnoreToken,
	// Something that is immediately ignores (not deleted)
	IgnoreTokenNoDelete,

	/// Classes ///
	ClassHeader,
	ClassBody,

	/// Expressions ///
	Expr,
	// An expression that stops being read at the end of the line
	// Used in cases like `return 1\n+1;`
	ExprEndAtNewline,
	ExprArray,
	ExprDict,
}

fn is_whitespace(t: &TokenType) -> bool {
	matches!(t, TokenType::Spacing) ||
	matches!(t, TokenType::Comment) ||
	matches!(t, TokenType::LineTerminator)
}

pub fn transform(tokens: &mut Vec<Token>) {
	let mut insert_tokens: Vec<(usize, Token)> = vec![];
	let mut remove_tokens: Vec<usize> = vec![];
	let mut index: usize = usize::MAX;
	let mut stack: Vec<FrameType> = vec![FrameType::TopLevel];
	loop {
		index = index.wrapping_add(1);
		if index == tokens.len() { break }

		let token = &tokens[index]; // Actual token
		let token_str: &str = token.token.as_str(); // Token string
		let token_is_whitespace = is_whitespace(&token.t);
		let stack_len = stack.len();
		let top_stack = stack.last()
			.unwrap_or(&FrameType::TopLevel).clone(); // The level we're on
		let top_stack_second = Clone::clone(if stack_len < 2 {
			&top_stack
		} else {
			&stack[stack_len - 2]
		});

		// Print (for debugging)
		let token_str_escaped = token_str.escape_debug().to_string();
		if token_str_escaped.len() <= 20 {
			println!(
				"`{}` -{}- Stack: {:?}",
				token_str_escaped,
				"-".repeat(20 - token_str_escaped.len()),
				stack
			);
		} else {
			println!(
				"`{}` - Stack: {:?}",
				token_str_escaped,
				stack
			);
		}

		// Remove some tokens immediately
		if REMOVE_KEYWORDS.contains(&token_str) {
			remove_tokens.push(index);
			remove_tokens.push(index + 1);
		} else if matches!(top_stack, FrameType::IgnoreToken) {
			remove_tokens.push(index);
			if !token_is_whitespace {
				// Keep ignoring and deleting everything until we find a
				// non-whitespace character.
				stack.pop();
			}
		} else if matches!(top_stack, FrameType::IgnoreTokenNoDelete) {
			if !token_is_whitespace {
				// Keep ignoring everything until we
				// find a non-whitespace character.
				stack.pop();
			}
		}

		// If we're already parsing a function...
		else if transform_function(
			&mut stack,
			&top_stack,
			&mut index,
			&token_str,
			token_is_whitespace,
			&mut remove_tokens
		) { continue; }

		// Types
		else if transform_type(
			&mut stack,
			&top_stack,
			&top_stack_second,
			&mut index,
			&tokens,
			&token_str,
			&token.t,
			token_is_whitespace,
			&mut remove_tokens
		) { continue; }

		// Match classes
		else if token_str == "class" {
			stack.push(FrameType::ClassHeader);
		} else if transform_class(
			&mut stack,
			&top_stack,
			&mut index,
			&token_str,
			token_is_whitespace,
			&mut remove_tokens
		) { continue; }

		// Match expressions
		else if token_str == "(" {
			stack.push(FrameType::Expr);
		} else if matches!(top_stack, FrameType::Expr)
			|| matches!(top_stack, FrameType::ExprEndAtNewline)
			|| matches!(top_stack, FrameType::ExprArray)
			|| matches!(top_stack, FrameType::ExprDict) {
			if token_is_whitespace {
				// Whitespace in expressions doesn't do anything
				continue;
			} else if token_str == "(" {
				stack.push(FrameType::Expr);
			} else if token_str == "[" {
				stack.push(FrameType::ExprArray);
			} else if token_str == "{" {
				stack.push(FrameType::ExprDict);
			} else if token_str == "\n" && matches!(top_stack, FrameType::ExprEndAtNewline) {
				stack.pop();
			} else if ";".contains(&token_str) {
				stack.pop();
			} else if CLOSING_BRACKETS.contains(token_str) {
				// index -= 1;
				stack.pop();
			}
		}

		// Type Declaration
		else if token_str == "type" {
			if index == tokens.len() || !matches!(tokens[index + 1].t, TokenType::Name) {
				// If we're at the end, or the following token isn't a name,
				// ignore this token.
			} else {
				// Since a `type` declaration is just [type] = [type], we can
				// push ::Type twice!
				remove_tokens.push(index);
				stack.push(FrameType::Type);
				stack.push(FrameType::IgnoreToken);
				stack.push(FrameType::Type);
			}
		}

		// Interfaces (entry point)
		else if token_str == "interface" {
			remove_tokens.push(index);
			stack.push(FrameType::InterfaceHeader);
		} else if transform_interface(
			&mut stack,
			&top_stack,
			&mut index,
			&token_str,
			&mut remove_tokens
		) { continue; }
		
		// Functions (entry point)
		else if token_str == "function" {
			stack.push(FrameType::FunctionName);
		}

		// Variables
		else if VARIABLE_DEFINITION.contains(&token_str) {
			stack.push(FrameType::VariableName);
		} else if transform_variable_definition(
			&mut stack,
			&top_stack,
			&mut index,
			&tokens,
			&token_str,
			token_is_whitespace,
			&mut remove_tokens,
		) { continue; }

		// Enums
		else if token_str == "enum" {
			remove_tokens.push(index);
			stack.push(FrameType::EnumHeader { name: None });
		} else if transform_enum(
			&mut stack,
			&top_stack,
			&mut index,
			&tokens,
			&token_str,
			&token.t,
			token_is_whitespace,
			&mut remove_tokens,
			&mut insert_tokens,
		) { continue; }

		// Type casting!
		else if token_str == "as" {
			remove_tokens.push(index);
			stack.push(FrameType::Type);
		}

		// Anything else!
		else {
			if token_str == "<" {
				println!("Getting generic range!");
				let generic_opt =
					get_generic_range(tokens, index);
				if let Some(generic) = generic_opt {
					println!("Found generic range!!!");
					for i in generic.start_index..generic.end_index {
						remove_tokens.push(i);
					}
				}
			} else if token_str == "{" {
				stack.push(FrameType::Block);
			} else if token_str == "}" {
				stack.pop();
			}
		}

	}

	let mut insert_index = 0;
	for i in 0..tokens.len() {
		let t = &tokens[i];
		if remove_tokens.contains(&i) {
			// print!("[{}]", t.token);
		} else {
			print!("{}", t.token);
		}
		while insert_index != insert_tokens.len() &&
			i == insert_tokens[insert_index].0 {
			print!("{}", insert_tokens[insert_index].1.token);
			insert_index += 1;
		}
	}
	println!();
}

struct GenericPosInfo {
	start_index: usize,
	end_index: usize,
}

fn get_generic_range(
	tokens: &Vec<Token>,
	start_index: usize,
) -> Option<GenericPosInfo> {
	if start_index == 0 {
		return None;
	}
	let mut index = start_index - 1;
	while is_whitespace(&tokens[index].t) {
		if index == 0 {
			return None;
		}
		index -= 1;
	}
	let mut stack: Vec<char> = vec![];
	index = start_index;
	loop {
		index += 1;
		if index >= tokens.len() { return None; }
		let token_str = tokens[index].token.as_str();
		// There aren't any empty tokens, so get the first char of each
		let token_char = token_str.chars().nth(0).unwrap();
		if token_str == "<" {
			stack.push('>');
		} else if token_str == "(" {
			stack.push(')');
		} else if token_str == "[" {
			stack.push(']');
		} else if token_str == "{" {
			stack.push('}');
		} else if ")]}>".contains(token_char) {
			// Found a closing token
			if stack.len() == 0 && token_char == '>' {
				index += 1;
				while index < tokens.len() {
					if !is_whitespace(&tokens[index].t) {
						if tokens[index].token != "(" {
							// If the next (non-whitespace) token is not an
							// opening parenthesis, it's not a generic
							return None;
						}
						break;
					}
					index += 1;
				}
				// Found THE closing token!
				return Some(GenericPosInfo {
					start_index,
					end_index: index
				});
			} else if stack.len() == 0 {
				break;
			} else if &token_char == stack.last().unwrap() {
				stack.pop();
			} else {
				break;
			}
		}
	}
	
	return None;
}

fn transform_interface(
	stack: &mut Vec<FrameType>,
	top_stack: &FrameType,
	index: &mut usize,
	token_str: &str,
	remove_tokens: &mut Vec<usize>,
) -> bool {
	match top_stack {
		FrameType::InterfaceHeader => {
			remove_tokens.push(*index);
			if ["extends", "implements"].contains(&token_str) {
				stack.push(FrameType::Type);
			} else if token_str == "{" {
				stack.pop();
				stack.push(FrameType::InterfaceBody);
			}
		}
		FrameType::InterfaceBody => {
			remove_tokens.push(*index);
			if token_str == "{" {
				stack.push(FrameType::InterfaceBody);
			} else if token_str == "}" {
				stack.pop();
			}
		},
		_ => { return false; }
	}
	return true;
}

fn transform_variable_definition(
	stack: &mut Vec<FrameType>,
	top_stack: &FrameType,
	index: &mut usize,
	tokens: &Vec<Token>,
	token_str: &str,
	token_is_whitespace: bool,
	remove_tokens: &mut Vec<usize>,
) -> bool {
	match top_stack {
		FrameType::VariableName => {
			if token_is_whitespace {
				return true;
			} else if token_str == "enum" {
				let mut back_idx = *index - 1;
				while !VARIABLE_DEFINITION.contains(
					&tokens[back_idx].token.as_str()
				) {
					back_idx -= 1;
				}
				while back_idx != *index + 1 {
					remove_tokens.push(back_idx);
					back_idx += 1;
				}
				stack.pop();
				stack.push(FrameType::EnumHeader {
					name: None
				});
				return true;
			}
			stack.pop();
			stack.push(FrameType::VariableAfterName);
		},
		FrameType::VariableAfterName => {
			if token_str == ":" {
				stack.pop();
				remove_tokens.push(*index);
				stack.push(FrameType::Type);
			} else if token_str == "=" {
				stack.pop();
			}
		},
		_ => { return false; }
	}
	return true;
}

fn transform_enum(
	stack: &mut Vec<FrameType>,
	top_stack: &FrameType,
	index: &mut usize,
	tokens: &Vec<Token>,
	token_str: &str,
	token_type: &TokenType,
	token_is_whitespace: bool,
	remove_tokens: &mut Vec<usize>,
	insert_tokens: &mut Vec<(usize, Token)>
) -> bool {
	let stack_len = stack.len();
	match top_stack {
		FrameType::EnumHeader { .. } => {
			remove_tokens.push(*index);
			if !token_is_whitespace {
				// We got the name!
				let mut name_empty = false;
				if let FrameType::EnumHeader {
					name
				} = &stack[stack_len - 1] {
					if name.is_none() {
						name_empty = true;
					}
				}
				if let FrameType::EnumHeader {
					ref mut name
				} = stack[stack_len - 1] {
					if name_empty {
						*name = Some(String::from(token_str));
						return true;
					}
				}
			}
			if token_str == "{" {
				if let FrameType::EnumHeader {
					name
				} = stack.last().unwrap() {
					// name = &Some(String::from(token_str));
					let final_name =
						String::from(name.as_ref().unwrap());
					stack.pop();
					stack.push(FrameType::EnumBody {
						name: final_name,
						entries: vec![],
						curr_name: String::new()
					});
				} else {
					panic!("This shouldn't happen! (1)");
				}
			}
		},
		FrameType::EnumBody { .. } => {
			remove_tokens.push(*index);
			if token_str == "=" {
				let mut out: Vec<Token> = vec![];
				*index += 1;
				while ![",", "\n","}"].contains(&tokens[*index].token.as_str()) {
					remove_tokens.push(*index);
					out.push(tokens[*index].clone());
					*index += 1;
				}
				*index -= 1;
				if let FrameType::EnumBody {
					name: _,
					ref mut entries,
					ref mut curr_name
				} = stack[stack_len - 1] {
					entries.push((curr_name.to_string(), out));
					*curr_name = String::new();
				}
			} else if token_str == "}" {
				let mut full_enum = String::new();

				// Add the last name (if it didn't have an explicit value)
				if let FrameType::EnumBody {
					ref name,
					ref mut entries,
					ref mut curr_name
				} = stack[stack_len - 1] {
					if curr_name.len() > 0 {
						entries.push((curr_name.to_string(), vec![]));
					}
					let name_str = name.as_str();
					full_enum += "let ";
					full_enum += name_str;
					full_enum += ";(()=>{";
					let mut last_value = String::from("0");
					for e in entries {
						let key_str = e.0.as_str();
						full_enum += name_str;
						full_enum += "[";
						full_enum += name_str;
						full_enum += "[\"";
						full_enum += key_str;
						full_enum += "\"]=";
						if e.1.len() == 0 {
							// TODO: fix this for strings!
							last_value += "+1";
						} else {
							last_value.clear();
							let mut i: usize = 0;
							while i < e.1.len() {
								last_value += &e.1[i].token;
								i += 1;
							}
						}
						full_enum += last_value.as_str();
						full_enum += "]=\"";
						full_enum += key_str;
						full_enum += "\";";
					}
					full_enum += "})(";
					full_enum += name_str;
					full_enum += "||(";
					full_enum += name_str;
					full_enum += "={}));";
				}

				// Insert the whole enum as a token
				insert_tokens.push((*index, Token {
					t: TokenType::GeneratedCode,
					token: full_enum,
					index: *index,
				}));
				stack.pop();
			} else if !token_is_whitespace && matches!(token_type, TokenType::Name) {
				if let FrameType::EnumBody {
					name: _,
					ref mut entries,
					ref mut curr_name
				} = stack[stack_len - 1] {
					if curr_name.len() > 0 {
						entries.push((curr_name.to_string(), vec![]));
					}
					*curr_name = String::from(token_str);
				}
			}
		},
		_ => { return false; }
	}
	return true;
}

fn transform_class(
	stack: &mut Vec<FrameType>,
	top_stack: &FrameType,
	index: &mut usize,
	token_str: &str,
	token_is_whitespace: bool,
	remove_tokens: &mut Vec<usize>
) -> bool {
	match top_stack {
		FrameType::ClassHeader => {
			if token_str == "<" {
				*index -= 1;
				stack.push(FrameType::TypeAfter);
			} else if token_str == "implements" {
				remove_tokens.push(*index);
				stack.push(FrameType::Type);
			} else if token_str == "{" {
				stack.pop();
				stack.push(FrameType::ClassBody);
			}
		},
		FrameType::ClassBody => {
			if token_is_whitespace || token_str == "static" {
				return true;
			}
			// Once we find a non-whitespace, non JS thing, decide what to do!
			if token_str == ":" {
				// Match property types
				remove_tokens.push(*index);
				stack.push(FrameType::Type);
			} else if token_str == "=" {
				*index -= 1;
				// Match property assignment
				stack.push(FrameType::Expr);
			} else if token_str == "<" {
				// Match function signature generics
				*index -= 1;
				stack.push(FrameType::FunctionName);
			} else if token_str == "(" {
				// Match function signature parameters
				stack.push(FrameType::FunctionParams);
			} else if token_str == "[" {
				// Match `[key: string]: string` index signature
				remove_tokens.push(*index);
				stack.push(FrameType::TypeIndexSignature);
			} else if token_str == "}" {
				// Match the end of the class
				stack.pop();
			}
		}
		_ => { return false; }
	}
	return true;
}

/// Parses a function. Returns true if it's still parsing it, false otherwise.
fn transform_function(
	stack: &mut Vec<FrameType>,
	top_stack: &FrameType,
	index: &mut usize,
	token_str: &str,
	token_is_whitespace: bool,
	remove_tokens: &mut Vec<usize>
) -> bool {
	match top_stack {
		FrameType::FunctionName => {
			if token_str == "<" {
				*index -= 1;
				stack.push(FrameType::TypeAfter);
			} else if token_str == "(" {
				stack.pop();
				stack.push(FrameType::FunctionParams);
			}
		},
		FrameType::FunctionParams => {
			// Function parameters
			if token_str == "?" {
				remove_tokens.push(*index);
			} else if token_str == ":" {
				remove_tokens.push(*index);
				stack.push(FrameType::Type);
			} else if token_str == "=" {
				// Default values aren't TypeScript code!
				// this makes `function fname(a = {v: 123})` possible
				//                                  ^^^^^
				stack.push(FrameType::Expr);
			} else if token_str == ")" {
				stack.pop();
				stack.push(FrameType::FunctionOptionalReturnType);
			}
		},
		FrameType::FunctionOptionalReturnType => {
			if token_is_whitespace {
				return true;
			} else if token_str == ";" {
				// This was a declaraction function
				stack.pop();
			} else if token_str == ":" {
				// The function does have a return type
				remove_tokens.push(*index);
				stack.pop();
				stack.push(FrameType::FunctionBody);
				stack.push(FrameType::IgnoreTokenNoDelete);
				stack.push(FrameType::Type);
			} else {
				stack.pop();
				stack.push(FrameType::FunctionBody);
			}
		},
		_ => { return false; }
	}
	return true;
}

fn transform_type(
	stack: &mut Vec<FrameType>,
	top_stack: &FrameType,
	top_stack_second: &FrameType,
	index: &mut usize,
	tokens: &Vec<Token>,
	token_str: &str,
	token_type: &TokenType,
	token_is_whitespace: bool,
	remove_tokens: &mut Vec<usize>
) -> bool {
	match top_stack {
		FrameType::Type => {
			remove_tokens.push(*index);
			if token_is_whitespace {
				// Keep going until we find something that ISN'T spacing
				return true;
			}
			stack.pop();
			if token_str == "(" {
				// Type in parenthesis (ex: `(Type<Something>)`)
				stack.push(FrameType::IgnoreToken); // Match the outer paren
				stack.push(FrameType::Type); // Match the following type
			} if token_str == "{" {
				// Dictionary (ex: `{ something: string }`)
				stack.push(FrameType::TypeDict);
			} if token_str == "[" {
				if matches!(top_stack_second, FrameType::TypeDict) {
					// Dictionary type signature
					stack.push(FrameType::TypeIndexSignature);
				} else {
					// Tuple (ex: `[number, number]`)
					stack.push(FrameType::TypeTupleOrIndex);
				}
			} else if ["keyof", "typeof"].contains(&token_str) {
				stack.push(FrameType::Type); // Match the following type
			} else if matches!(token_type, TokenType::String) {
				// String (ex: `'nice'`)
				stack.push(FrameType::TypeAfter);
			} else if matches!(token_type, TokenType::Name) {
				// Word (ex: `Type<Inner>`, `Type`)
				stack.push(FrameType::TypeAfter);
			}
		},
		FrameType::TypeAfter => {
			// This runs *after* a type. It makes sure that anything 
			if token_is_whitespace {
				remove_tokens.push(*index);
			} else if token_str == "<" {
				remove_tokens.push(*index);
				stack.push(FrameType::TypeInfoEndOrSeparator);
				stack.push(FrameType::Type);
			} else if token_str == ">" {
				*index -= 1;
				remove_tokens.push(*index);
				stack.pop();
			} else if ["&", "|", "extends", "?", ":"].contains(&token_str) {
				// Type joining!
				remove_tokens.push(*index);
				stack.pop();
				stack.push(FrameType::Type);
			} else if token_str == "[" {
				// Tuple or index (ex: `[number, number]`, `Value[index]`)
				remove_tokens.push(*index);
				stack.pop();
				stack.push(FrameType::TypeTupleOrIndex);
			} else {
				*index -= 1;
				stack.pop();
				if !matches!(stack.last().unwrap(), FrameType::TypeAfter) &&
					!matches!(stack.last().unwrap(), FrameType::IgnoreToken) {
					// Reached the end of the type! Finally!
					// Now, add all the trailing whitespace back.
					while is_whitespace(&tokens[*remove_tokens.last().unwrap()].t) {
						remove_tokens.pop();
					}
				}
			}
		},
		FrameType::TypeInfoEndOrSeparator => {
			// remove_tokens.push(index);
			if token_str == ">" {
				remove_tokens.push(*index);
				stack.pop();
			} else if token_str == "," {
				remove_tokens.push(*index);
				stack.push(FrameType::Type);
			} else {
				remove_tokens.push(*index);
			}
		},
		FrameType::TypeDict => {
			// We found a typed dict/object! Ignore it.
			if token_is_whitespace {
				remove_tokens.push(*index);
				return true;
			} else if token_str == "}" {
				remove_tokens.push(*index);
				stack.pop();
				stack.push(FrameType::TypeAfter);
			} else {
				*index -= 1;
				stack.push(FrameType::Type);
			}
		},
		FrameType::TypeTupleOrIndex => {
			// We found a typed array (a tuple)! Ignore it.
			if token_is_whitespace {
				remove_tokens.push(*index);
				return true;
			} else if token_str == "]" {
				remove_tokens.push(*index);
				stack.pop();
				stack.push(FrameType::TypeAfter);
			} else {
				*index -= 1;
				stack.push(FrameType::Type);
			}
		},
		FrameType::TypeIndexSignature => {
			// Matches `[key: string]: string`
			remove_tokens.push(*index);
			if token_str == ":" {
				stack.push(FrameType::Type);
			} else if token_str == "]" {
				stack.pop();
				stack.push(FrameType::Type);
				stack.push(FrameType::IgnoreToken);
			}
		},
		_ => { return false; }
	}
	return true;
}