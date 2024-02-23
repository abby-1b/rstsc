use crate::tokenizer::{Token, TokenType};

const SINGLE_EXPRESSION_KEYWORDS: [&str; 6] = [
	"continue",
	"break",
	"return",
	"throw",
	"yield",
	"debugger"
];

const VARIABLE_DEFINITION: [&str; 3] = [ "const", "let", "var" ];
const REMOVE_KEYWORDS: [&str; 4] = [
	"private",
	"public",
	"abstract",
	"protected"
];
const CLOSING_BRACKETS: &'static str = ")]}";

const VISIBILITY_MODIFIERS: [&str; 1] = [ "export" ];

#[derive(Debug, Clone)]
enum FrameType {
	TopLevel,
	Block,

	// Functions //
	FunctionName,
	FunctionParams,
	FunctionOptionalReturnType,
	FunctionBody,

	// For Loops //
	/// When the `for` keyword is found
	ForLoopDeclaration,
	/// When we reach the inner parenthesis of the for loop
	ForLoopInsides,

	// Variables //
	VariableName,
	VariableAfterName,

	// Types //
	Type,
	TypeAfter,
	TypeDict,
	TypeTupleOrIndex,
	TypeInfoEndOrSeparator,
	TypeIndexSignature,
	TypeDeclarationPotential { start: usize },

	// Interfaces //
	InterfaceHeader,
	InterfaceBody,

	// Enums //
	EnumHeader { name: Option<String> },
	EnumBody {
		name: String,
		entries: Vec<(String, Vec<Token>)>,
		curr_name: String,
	},

	// Declarations //
	/// Immediately after the `declaration` keyword
	DeclarationHeader,
	/// Immediately after a dot is found in the declaration header
	DeclarationHeaderIgnoreAfterDot,
	/// Everything (getting ignored) until the end of the declaration
	DeclarationBody { simple: bool, start_index: usize },

	/// Something that is immediately ignored (and deleted)
	IgnoreToken,
	/// Something that is immediately ignored (not deleted)
	IgnoreTokenNoDelete,

	// Classes //
	ClassHeader,
	ClassBody,

	// Statements //
	StatementSoft,
	StatementHard,

	// Expressions //
	Expr,
	/// An expression that stops being read at the end of the line
	/// Used in cases like `return 1\n+1;`
	ExprSoft { ends_at: String },
	ExprArray,
	ExprDict,

	/// Ternary + Conditional Chaining ///
	TernaryOrConditionalChaining,
}

impl PartialEq for FrameType {
	fn eq(&self, other: &Self) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }
}

/// Frames where only expressions are allowed (blocks are dicts, variables don't work, etc.)
const EXPRESSION_FRAMES: [FrameType; 4] = [
	FrameType::Expr,
	FrameType::ExprSoft { ends_at: String::new() },
	FrameType::ExprArray,
	FrameType::ExprDict
];

/// Frames that are exited as soon as another is entered
const SOFT_FRAMES: [FrameType; 1] = [
	FrameType::StatementSoft
];

fn is_whitespace(t: &TokenType) -> bool {
	matches!(t, TokenType::Spacing) ||
	matches!(t, TokenType::Comment) ||
	matches!(t, TokenType::LineTerminator)
}

fn is_expr_soft_and_has(e: &FrameType, c: &str) -> bool {
	match e {
		FrameType::ExprSoft { ends_at } => {
			ends_at.contains(c)
		},
		_ => false
	}
}

pub fn transform(tokens: Vec<Token>) -> Vec<Token> {
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

		// let token_second = if index == 0 {
		// 	token
		// } else {
		// 	&tokens[index - 1]
		// };
		// let token_second_str = token_second.token.as_str();

		let stack_len = stack.len();
		let top_stack = stack.last()
			.unwrap_or(&FrameType::TopLevel).clone(); // The level we're on
		let top_stack_second = Clone::clone(if stack_len < 2 {
			&top_stack
		} else {
			&stack[stack_len - 2]
		});

		if SOFT_FRAMES.contains(&top_stack_second) {
			let top = stack.pop();
			stack.pop();
			stack.push(top.unwrap());
		}

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

		// Declarations
		else if transform_declaration(
			&mut stack,
			&top_stack,
			&mut index,
			&token_str,
			&mut remove_tokens
		) { continue; }

		// If we're already parsing a function...
		else if transform_function(
			&mut stack,
			&top_stack,
			&mut index,
			&token_str,
			token_is_whitespace,
			&mut remove_tokens
		) { continue; }

		else if transform_for(
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

		// Interfaces
		else if transform_interface(
			&mut stack,
			&top_stack,
			&mut index,
			&token_str,
			&mut remove_tokens
		) { continue; }

		// Enums
		else if transform_enum(
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

		else if SINGLE_EXPRESSION_KEYWORDS.contains(&token_str) {
			stack.push(FrameType::ExprSoft { ends_at: String::from(";\n") });
		}

		// Match arrow functions
		else if token_str == "=>" {
			stack.push(FrameType::StatementSoft);
		}

		// Match different expressions
		else if token_str == "(" {
			stack.push(FrameType::Expr);
		} else if token_str == "[" {
			stack.push(FrameType::ExprArray);
		} else if token_str == "{" {
			if EXPRESSION_FRAMES.contains(&top_stack) {
				// A `{` in an expression means a dict
				stack.push(FrameType::ExprDict);
			} else {
				// A `{` in a non-expression means a block
				stack.push(FrameType::Block);
			}
		} else if is_expr_soft_and_has(&top_stack, token_str) {
			stack.pop();
		} else if token_str == "as" {
			remove_tokens.push(index);
			stack.push(FrameType::Type);
		} else if token_str == ":" {
			if matches!(top_stack, FrameType::ExprDict) {
				// A `:` in a dict means we're now in a soft expression!
				// stack.push(FrameType::ExprSoft { endsat_newline: false, endsat_comma: true });
				stack.push(FrameType::ExprSoft { ends_at: String::from(",") });
			} else {
				// A `:` outside a dict means a type!
				remove_tokens.push(index);
				stack.push(FrameType::Type);
			}
		} else if CLOSING_BRACKETS.contains(token_str) {
			if matches!(top_stack, FrameType::ExprSoft { .. }) {
				index -= 1;
			}
			stack.pop();
		} else if (
			matches!(top_stack, FrameType::Expr) ||
			matches!(top_stack, FrameType::ExprSoft { .. })
		) && ";".contains(&token_str) {
			index -= 1;
			// End some expressions when `;` is encountered
			stack.pop();
		}
		
		// Deal with hard statements (statements ended with `;`)
		else if matches!(top_stack, FrameType::StatementHard) && ";".contains(&token_str) {
			// End hard statements when `;` is encountered
			stack.pop();
		}

		else if EXPRESSION_FRAMES.contains(&top_stack) {
			
		}

		// Ternary + conditional chaining
		else if token_str == "?" {
			stack.push(FrameType::TernaryOrConditionalChaining);
		} else if transform_ternary_and_conditional_chaining(
			&mut stack,
			&top_stack,
			&mut index,
			&token_str,
			token_is_whitespace,
		) { continue; }

		// Non-null assertion
		else if token_str == "!" {
			if index == 0 { continue; }
			let last_token = &tokens[index - 1];
			if matches!(last_token.t,
				TokenType::Name | TokenType::Number | TokenType::String
			) || CLOSING_BRACKETS.contains(last_token.token.as_str()) {
				remove_tokens.push(index);
			}
		}

		// Return statement
		else if token_str == "return" {
			stack.push(FrameType::ExprSoft { ends_at: String::from("\n") });
		}

		// Switch statement
		else if [ "case", "default" ].contains(&token_str) {
			// Basically ignore the ':' as a type
			// Very sketchy workaround but it works!
			stack.push(FrameType::ExprSoft { ends_at: String::from(":") });
		}

		// Type Declaration
		else if token_str == "type" {
			if index == tokens.len() - 1 {
				// If we're at the end, ignore this token
				continue;
			}

			stack.push(FrameType::TypeDeclarationPotential { start: index });
		} else if matches!(top_stack, FrameType::TypeDeclarationPotential { .. }) {
			if token_is_whitespace && token_str != "\n" {
				continue;
			} else if matches!(token.t, TokenType::Name) {
				// It's an actual type!
				stack.pop();
				// Since a `type` declaration is just `type [type] = [type]``,
				// we can push ::Type twice! (and don't forget to ignore the =)
				if let FrameType::TypeDeclarationPotential {
					start
				} = top_stack {
					remove_potential_visibility_modifier(
						&tokens,
						start - 1,
						&mut remove_tokens,
					);
					let mut i = start;
					while i < index {
						remove_tokens.push(i);
						i += 1;
					}
				}
				index -= 1;
				stack.push(FrameType::Type);
				stack.push(FrameType::IgnoreToken);
				stack.push(FrameType::Type);
			} else {
				// Set the index back to right after the `type` keyword
				if let FrameType::TypeDeclarationPotential {
					start
				} = top_stack {
					index = start;
				}
				stack.pop();
			}
		}

		// Declare
		else if token_str == "declare" {
			if index > 0 {
				remove_potential_visibility_modifier(
					&tokens,
					index - 1,
					&mut remove_tokens,
				);
			}
			remove_tokens.push(index);
			if [ "module", "namespace" ].contains(&tokens[index + 1].token.as_str()) {
				index += 1;
				remove_tokens.push(index);
			}
			stack.push(FrameType::DeclarationHeader);
		}

		// Interfaces (entry point)
		else if token_str == "interface" {
			if index > 0 {
				remove_potential_visibility_modifier(
					&tokens,
					index - 1,
					&mut remove_tokens,
				);
			}
			remove_tokens.push(index);
			stack.push(FrameType::InterfaceHeader);
		}
		
		// Functions (entry point)
		else if token_str == "function" {
			stack.push(FrameType::FunctionName);
		}

		// For loops!
		else if token_str == "for" {
			stack.push(FrameType::ForLoopDeclaration);
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
		}

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
					get_generic_range(&tokens, index);
				if let Some(generic) = generic_opt {
					println!("Found generic range!!!");
					for i in generic.start_index..generic.end_index {
						remove_tokens.push(i);
					}
				}
			}
		}

	}

	let mut out_tokens: Vec<Token> = Vec::with_capacity(
		tokens.len() - remove_tokens.len() + insert_tokens.len()
	);

	let mut insert_index = 0;
	for i in 0..tokens.len() {
		let t = &tokens[i];
		if remove_tokens.contains(&i) {
			// print!("[{}]", t.token);
		} else {
			out_tokens.push(t.clone());
			// print!("{}", t.token);
		}
		while insert_index != insert_tokens.len() &&
			i == insert_tokens[insert_index].0 {
			out_tokens.push(insert_tokens[insert_index].1.clone());
			// print!("{}", insert_tokens[insert_index].1.token);
			insert_index += 1;
		}
	}
	// println!();

	return out_tokens;
}

fn remove_potential_visibility_modifier(
	tokens: &Vec<Token>,
	index: usize,
	remove_tokens: &mut Vec<usize>,
) {
	// Get the first name token behind the current one
	let mut i = index;
	while is_whitespace(&tokens[i].t) {
		if i == 0 { return; }
		i -= 1;
	}
	let token_str = tokens[i].token.as_str();

	// Stop if it's not a visibility modifier
	if !VISIBILITY_MODIFIERS.contains(&token_str) {
		return;
	}

	while i != index {
		remove_tokens.push(i);
		i += 1;
	}
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
			if [ "extends", "implements" ].contains(&token_str) {
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

fn transform_declaration(
	stack: &mut Vec<FrameType>,
	top_stack: &FrameType,
	index: &mut usize,
	token_str: &str,
	remove_tokens: &mut Vec<usize>,
) -> bool {
	match top_stack {
		FrameType::DeclarationHeader => {
			remove_tokens.push(*index);
			if token_str == "." {
				// If we find a dot, go into this special state...
				stack.pop();
				stack.push(FrameType::DeclarationHeaderIgnoreAfterDot);
			} else if token_str == "{" {
				// Simple declaration bodies just exit after their first nest
				// a.k.a. `declaration global {}` exits after the closing `}`
				stack.pop();
				stack.push(FrameType::DeclarationBody {
					simple: true,
					start_index: 0,
				});
			} else if VARIABLE_DEFINITION.contains(&token_str)
				|| token_str == "function" {
				// We'll remove this token later!
				remove_tokens.pop();
				*index -= 1;

				stack.pop();
				stack.push(FrameType::DeclarationBody {
					simple: false,
					start_index: *index,
				});
			}
		},
		FrameType::DeclarationHeaderIgnoreAfterDot => {
			remove_tokens.push(*index);
			if token_str == "{" {
				stack.pop();
				stack.push(FrameType::DeclarationBody {
					simple: true,
					start_index: 0,
				});
			}
		},
		FrameType::DeclarationBody { simple, start_index } => {
			if *simple {
				remove_tokens.push(*index);
				if token_str == "{" {
					stack.push(FrameType::DeclarationBody {
						simple: true,
						start_index: 0,
					});
				} else if token_str == "}" {
					stack.pop();
				}
			} else {
				if *index > *start_index + 1 {
					// Ignore all the removed tokens, as we'll be adding them in ourselves
					let start_removing_from = *start_index + 1;
					while remove_tokens.len() > 0 && *unsafe {
						remove_tokens.last().unwrap_unchecked()
					} > start_removing_from { remove_tokens.pop(); }
					for i in start_removing_from..=*index {
						remove_tokens.push(i);
					}
					stack.pop();
				}
				return false;
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
				stack.push(FrameType::Expr);
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
			else if token_str == "?" {
				// Match nullable properties
				remove_tokens.push(*index);
			} if token_str == ":" {
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
				stack.push(FrameType::ExprSoft { ends_at: String::from(",") });
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
				stack.push(FrameType::TypeAfter); // Match the stuff after
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
			} else if token_str == ")" {
				// Apparently the type ended immediately! (ex: `() => void`)
				*index -= 1;
			}
		},
		FrameType::TypeAfter => {
			// This runs *after* a type. It makes sure that anything 
			if token_is_whitespace {
				remove_tokens.push(*index);
			} else if token_str == "=>" {
				remove_tokens.push(*index);
				stack.pop();
				stack.push(FrameType::Type);
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

fn transform_ternary_and_conditional_chaining(
	stack: &mut Vec<FrameType>,
	top_stack: &FrameType,
	index: &mut usize,
	token_str: &str,
	token_is_whitespace: bool,
) -> bool {
	match top_stack {
		FrameType::TernaryOrConditionalChaining => {
			if token_is_whitespace {
				return true;
			} else if token_str == "." {
				// It's conditional chaining, so leave it alone.
				*index -= 1;
				stack.pop();
			} else {
				// It's actually a ternary expression! That's just a soft
				// expression (ended with ":") and a normal expression after!
				*index -= 1;
				stack.pop();
				stack.push(FrameType::Expr);
				stack.push(FrameType::ExprSoft { ends_at: String::from(":") });
			}
		}
		_ => { return false; }
	}
	return true;
}

fn transform_for(
	stack: &mut Vec<FrameType>,
	top_stack: &FrameType,
	index: &mut usize,
	token_str: &str,
	token_is_whitespace: bool,
	remove_tokens: &mut Vec<usize>
) -> bool {
	match top_stack {
		FrameType::ForLoopDeclaration => {
			if token_str == "(" {
				stack.pop();
				stack.push(FrameType::ForLoopInsides);
			}
		},
		FrameType::ForLoopInsides => {
			if token_is_whitespace {
				return true;
			} else if token_str == ":" {
				// Normal for loop (let i: type = x; i < x; i++)
				remove_tokens.push(*index);
				stack.pop();
				stack.push(FrameType::StatementHard);
				stack.push(FrameType::StatementHard);
				stack.push(FrameType::StatementHard);
				stack.push(FrameType::Expr);
				stack.push(FrameType::Type)
			} else if token_str == "=" {
				stack.pop();
				stack.push(FrameType::StatementHard);
				stack.push(FrameType::StatementHard);
				stack.push(FrameType::StatementHard);
				stack.push(FrameType::Expr);
			} else if token_str == "of" || token_str == "in" {
				stack.pop();
				stack.push(FrameType::Expr);
			}
		},
		_ => { return false; }
	}
	return true;
}
