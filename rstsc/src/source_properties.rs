use crate::ast::{ASTArena, ASTIndex, ASTNode, ClassMember, ImportDefinition, ObjectProperty};
use crate::declaration::{DestructurableDeclaration, DestructurePattern};
use crate::symbol_table::SymbolTable;
use crate::tokenizer::TokenList;

#[derive(Debug, Clone, Copy)]
pub enum SMSrc {
  // Empty, so doesn't matter
  None,

  // Points to SourceProperties.source
  Source,

  // Points to SourceProperties.string_pool
  Pool,
}

#[derive(Debug, Clone, Copy)]
pub struct SrcMapping {
  pub idx: u32,
  pub len: u32,
  pub from: SMSrc,
}
impl SrcMapping {
  pub const fn empty() -> Self {
    SrcMapping { idx: 0, len: 0, from: SMSrc::None }
  }

  pub fn is_empty(&self) -> bool { self.len == 0 }
}

pub struct SourceProperties<'a> {
  pub source: &'a str,
  pub tokens: TokenList<'a>,
  pub st: SymbolTable,
  pub nodes: ASTArena,
  pub string_pool: String,
}

impl<'a> SourceProperties<'a> {
  pub fn new(source: &'a str) -> Self {
    SourceProperties {
      source,
      tokens: TokenList::from(source),
      st: SymbolTable::new(),
      nodes: ASTArena::new(),
      string_pool: String::new(),
    }
  }

  pub fn add_custom_string(&mut self, string: &str) -> SrcMapping {
    let idx = self.string_pool.len() as u32;
    let len = string.len() as u32;
    self.string_pool += string;
    SrcMapping { idx, len, from: SMSrc::Pool }
  }

  pub fn map_source(source: &str, mapping: SrcMapping) -> &str {
    // println!("[START]{}[END]", source);
    &source[mapping.idx as usize..(mapping.idx + mapping.len) as usize]
  }

  pub fn str_src<'b>(&'b self, mapping: SrcMapping) -> &'a str where 'a: 'b {
    let src = match mapping.from {
      SMSrc::Source => self.source,
      SMSrc::Pool => &self.string_pool,
      _ => panic!("Can't map; unknown source"),
    } as *const str;

    // Weird cast workaround!
    // We're assuming the user of this function won't use the str after adding
    // to the string pool, which could invalidate a reference to it...
    // If dropped memory is ever accessed, it's probably an issue here.
    Self::map_source(unsafe { &*src }, mapping)
  }

  pub fn print_ast_debug(&self, node_index: ASTIndex) {
    let output = self.format_ast_debug(node_index);
    println!("{}", output);
  }

  pub fn format_ast_debug(&self, node_index: ASTIndex) -> String {
    let mut output = String::new();
    self.print_ast_node_debug(node_index, 0, &mut output);
    output
  }

  /// Helper method that recursively prints a single AST node and its children
  fn print_ast_node_debug(
    &self,
    node_index: ASTIndex,
    indent_level: usize,
    output: &mut String
  ) {
    let node = self.nodes.get(node_index);
    let indent = "  ".repeat(indent_level);

    // Print the node type and basic info using a hybrid approach
    use ASTNode::*;
    match node {
      // Special handling for key nodes that need more detailed information
      Block { nodes } => {
        output.push_str(&format!("{}Block ({} children)\n", indent, nodes.len()));
        for child_index in nodes {
          self.print_ast_node_debug(*child_index, indent_level + 1, output);
        }
      }
      VariableDeclaration { modifiers, def_type, defs } => {
        let modifiers_str = if modifiers.is_empty() { "" } else { &modifiers.emit(true) };
        output.push_str(&format!("{}VariableDeclaration {}{} ({} definitions)\n", 
          indent, modifiers_str, def_type.emit(), defs.len()));
        for def in defs {
          self.print_destructurable_declaration_debug(def, indent_level + 1, output);
        }
      }
      FunctionDefinition { inner } => {
        let name = inner.name.map(|n| self.str_src(n)).unwrap_or("<anonymous>");
        let modifiers_str = if inner.modifiers.is_empty() { "" } else { &inner.modifiers.emit(true) };
        output.push_str(&format!("{}FunctionDefinition {}{}{}\n", 
          indent, modifiers_str, if modifiers_str.is_empty() { "" } else { " " }, name));
        output.push_str(&format!("{}  Generics: {} params\n", indent, inner.generics.len()));
        output.push_str(&format!("{}  Parameters: {} params\n", indent, inner.params.len()));
        if let Some(body) = &inner.body {
          output.push_str(&format!("{}  Body:\n", indent));
          self.print_ast_node_debug(*body, indent_level + 2, output);
        }
      }
      ArrowFunctionDefinition { inner } => {
        output.push_str(&format!("{}ArrowFunctionDefinition (async: {})\n", 
          indent, inner.is_async));
        output.push_str(&format!("{}  Generics: {} params\n", indent, inner.generics.len()));
        output.push_str(&format!("{}  Parameters: {} params\n", indent, inner.params.len()));
        output.push_str(&format!("{}  Body:\n", indent));
        self.print_ast_node_debug(inner.body, indent_level + 2, output);
      }
      ClassDefinition { inner } => {
        let name = inner.name.map(|n| self.str_src(n)).unwrap_or("<anonymous>");
        let modifiers_str = if inner.modifiers.is_empty() { "" } else { &inner.modifiers.emit(true) };
        output.push_str(&format!("{}ClassDefinition {}{}{}\n", 
          indent, modifiers_str, if modifiers_str.is_empty() { "" } else { " " }, name));
        if let Some(extends) = &inner.extends {
          output.push_str(&format!("{}  Extends: {}\n", indent, extends.get_single_name()));
        }
        output.push_str(&format!("{}  Members: {} members\n", indent, inner.members.len()));
        for member in &inner.members {
          self.print_class_member_debug(member, indent_level + 1, output);
        }
      }
      ExprIdentifier { name } => {
        output.push_str(&format!("{}ExprIdentifier: {}\n", indent, self.str_src(*name)));
      }
      ExprNumLiteral { number } => {
        output.push_str(&format!("{}ExprNumLiteral: {}\n", indent, self.str_src(*number)));
      }
      ExprStrLiteral { string } => {
        output.push_str(&format!("{}ExprStrLiteral: {}\n", indent, self.str_src(*string)));
      }
      ExprBoolLiteral { value } => {
        output.push_str(&format!("{}ExprBoolLiteral: {}\n", indent, value));
      }
      ExprFunctionCall { inner } => {
        output.push_str(&format!("{}ExprFunctionCall ({} args)\n", indent, inner.arguments.len()));
        output.push_str(&format!("{}  Callee:\n", indent));
        self.print_ast_node_debug(inner.callee, indent_level + 2, output);
        for (i, arg) in inner.arguments.iter().enumerate() {
          output.push_str(&format!("{}  Arg {}:\n", indent, i));
          self.print_ast_node_debug(*arg, indent_level + 3, output);
        }
      }
      ExprTernary { condition, if_true, if_false } => {
        output.push_str(&format!("{}ExprTernary\n", indent));
        output.push_str(&format!("{}  Condition:\n", indent));
        self.print_ast_node_debug(*condition, indent_level + 2, output);
        output.push_str(&format!("{}  If True:\n", indent));
        self.print_ast_node_debug(*if_true, indent_level + 2, output);
        output.push_str(&format!("{}  If False:\n", indent));
        self.print_ast_node_debug(*if_false, indent_level + 2, output);
      }
      PrefixOpr { opr, expr } => {
        output.push_str(&format!("{}PrefixOpr: {}\n", indent, self.str_src(*opr)));
        output.push_str(&format!("{}  Expression:\n", indent));
        self.print_ast_node_debug(*expr, indent_level + 2, output);
      }
      InfixOpr { left_right, opr } => {
        output.push_str(&format!("{}InfixOpr: {}\n", indent, self.str_src(*opr)));
        output.push_str(&format!("{}  Left:\n", indent));
        self.print_ast_node_debug(left_right.0, indent_level + 2, output);
        output.push_str(&format!("{}  Right:\n", indent));
        self.print_ast_node_debug(left_right.1, indent_level + 2, output);
      }
      PostfixOpr { expr, opr } => {
        output.push_str(&format!("{}PostfixOpr: {}\n", indent, self.str_src(*opr)));
        output.push_str(&format!("{}  Expression:\n", indent));
        self.print_ast_node_debug(*expr, indent_level + 2, output);
      }
      Parenthesis { nodes } => {
        output.push_str(&format!("{}Parenthesis ({} nodes)\n", indent, nodes.len()));
        for child_index in nodes {
          self.print_ast_node_debug(*child_index, indent_level + 1, output);
        }
      }
      Array { nodes } => {
        output.push_str(&format!("{}Array ({} elements)\n", indent, nodes.len()));
        for child_index in nodes {
          self.print_ast_node_debug(*child_index, indent_level + 1, output);
        }
      }
      Dict { properties } => {
        output.push_str(&format!("{}Dict ({} properties)\n", indent, properties.len()));
        for property in properties {
          self.print_object_property_debug(property, indent_level + 1, output);
        }
      }
      StatementIf { condition, body, alternate } => {
        output.push_str(&format!("{}StatementIf\n", indent));
        output.push_str(&format!("{}  Condition:\n", indent));
        self.print_ast_node_debug(*condition, indent_level + 2, output);
        output.push_str(&format!("{}  Body:\n", indent));
        self.print_ast_node_debug(*body, indent_level + 2, output);
        if let Some(alternate) = alternate {
          output.push_str(&format!("{}  Else:\n", indent));
          self.print_ast_node_debug(*alternate, indent_level + 2, output);
        }
      }
      StatementReturn { value } => {
        output.push_str(&format!("{}StatementReturn\n", indent));
        if let Some(value) = value {
          output.push_str(&format!("{}  Value:\n", indent));
          self.print_ast_node_debug(*value, indent_level + 2, output);
        }
      }
      StatementWhile { condition, body } => {
        output.push_str(&format!("{}StatementWhile\n", indent));
        output.push_str(&format!("{}  Condition:\n", indent));
        self.print_ast_node_debug(*condition, indent_level + 2, output);
        output.push_str(&format!("{}  Body:\n", indent));
        self.print_ast_node_debug(*body, indent_level + 2, output);
      }
      StatementFor { init, condition, update, body } => {
        output.push_str(&format!("{}StatementFor\n", indent));
        output.push_str(&format!("{}  Init:\n", indent));
        self.print_ast_node_debug(*init, indent_level + 2, output);
        output.push_str(&format!("{}  Condition:\n", indent));
        self.print_ast_node_debug(*condition, indent_level + 2, output);
        output.push_str(&format!("{}  Update:\n", indent));
        self.print_ast_node_debug(*update, indent_level + 2, output);
        output.push_str(&format!("{}  Body:\n", indent));
        self.print_ast_node_debug(*body, indent_level + 2, output);
      }
      StatementForOf { init, expression, body } => {
        output.push_str(&format!("{}StatementForOf\n", indent));
        output.push_str(&format!("{}  Init:\n", indent));
        self.print_ast_node_debug(*init, indent_level + 2, output);
        output.push_str(&format!("{}  Expression:\n", indent));
        self.print_ast_node_debug(*expression, indent_level + 2, output);
        output.push_str(&format!("{}  Body:\n", indent));
        self.print_ast_node_debug(*body, indent_level + 2, output);
      }
      StatementForIn { init, expression, body } => {
        output.push_str(&format!("{}StatementForIn\n", indent));
        output.push_str(&format!("{}  Init:\n", indent));
        self.print_ast_node_debug(*init, indent_level + 2, output);
        output.push_str(&format!("{}  Expression:\n", indent));
        self.print_ast_node_debug(*expression, indent_level + 2, output);
        output.push_str(&format!("{}  Body:\n", indent));
        self.print_ast_node_debug(*body, indent_level + 2, output);
      }
      InterfaceDeclaration { inner } => {
        let name = self.str_src(inner.name);
        let modifiers_str = if inner.modifiers.is_empty() { "" } else { &inner.modifiers.emit(true) };
        output.push_str(&format!("{}InterfaceDeclaration {}{}{}\n", 
          indent, modifiers_str, if modifiers_str.is_empty() { "" } else { " " }, name));
        output.push_str(&format!("{}  Generics: {} params\n", indent, inner.generics.len()));
        output.push_str(&format!("{}  Extends: {} types\n", indent, inner.extends.len()));
        // For the equals_type, we'll show a simplified representation
        output.push_str(&format!("{}  Type: [simplified type representation]\n", indent));
      }
      EnumDeclaration { inner } => {
        let name = self.str_src(inner.name);
        let modifiers_str = if inner.modifiers.is_empty() { "" } else { &inner.modifiers.emit(true) };
        output.push_str(&format!("{}EnumDeclaration {}{}{} (const: {})\n", 
          indent, modifiers_str, if modifiers_str.is_empty() { "" } else { " " }, name, inner.is_const));
        output.push_str(&format!("{}  Members: {} members\n", indent, inner.members.len()));
        for (member_name, member_value) in &inner.members {
          output.push_str(&format!("{}    {}:\n", indent, self.str_src(*member_name)));
          self.print_ast_node_debug(*member_value, indent_level + 3, output);
        }
      }
      StatementImport { inner } => {
        match &**inner {
          ImportDefinition::DefaultAliased { source, alias } => {
            output.push_str(&format!("{}StatementImport DefaultAliased\n", indent));
            output.push_str(&format!("{}  Source: {}\n", indent, self.str_src(*source)));
            output.push_str(&format!("{}  Alias: {}\n", indent, self.str_src(*alias)));
          }
          ImportDefinition::AllAsAlias { source, alias } => {
            output.push_str(&format!("{}StatementImport AllAsAlias\n", indent));
            output.push_str(&format!("{}  Source: {}\n", indent, self.str_src(*source)));
            output.push_str(&format!("{}  Alias: {}\n", indent, self.str_src(*alias)));
          }
          ImportDefinition::Individual { source, parts } => {
            output.push_str(&format!("{}StatementImport Individual ({} parts)\n", indent, parts.len()));
            output.push_str(&format!("{}  Source: {}\n", indent, self.str_src(*source)));
            for part in parts {
              output.push_str(&format!("{}  - {}", indent, self.str_src(part.name)));
              if let Some(alias) = part.alias {
                output.push_str(&format!(" as {}", self.str_src(alias)));
              }
              output.push_str("\n");
            }
          }
          ImportDefinition::SourceOnly { source } => {
            output.push_str(&format!("{}StatementImport SourceOnly\n", indent));
            output.push_str(&format!("{}  Source: {}\n", indent, self.str_src(*source)));
          }
        }
      }
      StatementExport { inner } => {
        output.push_str(&format!("{}StatementExport ({} specifiers)\n", indent, inner.specifiers.len()));
        for specifier in &inner.specifiers {
          output.push_str(&format!(
            "{}  - {}{}", indent,
            if specifier.is_type { "type " } else { "" },
            self.str_src(specifier.name)
          ));
          if let Some(alias) = specifier.alias {
            output.push_str(&format!(" as {}", self.str_src(alias)));
          }
          output.push_str("\n");
        }
      }
      ExprTemplateLiteral { inner } => {
        output.push_str(&format!("{}ExprTemplateLiteral\n", indent));
        output.push_str(&format!("{}  Head: {}\n", indent, self.str_src(inner.head)));
        output.push_str(&format!("{}  Parts: {} interpolations\n", indent, inner.parts.len()));
        for (i, (expr, literal)) in inner.parts.iter().enumerate() {
          output.push_str(&format!("{}    Part {}:\n", indent, i));
          output.push_str(&format!("{}      Expression:\n", indent));
          self.print_ast_node_debug(*expr, indent_level + 3, output);
          output.push_str(&format!("{}      Literal: {}\n", indent, self.str_src(*literal)));
        }
      }
      ExprRegexLiteral { inner } => {
        output.push_str(&format!("{}ExprRegexLiteral: /{}/{}\n", indent, 
          self.str_src(inner.pattern), self.str_src(inner.flags)));
      }
      // For other nodes, use the Debug trait to automatically handle new variants
      _ => {
        // Use the node's name() method for the type and Debug for fields
        let node_name = node.name();
        let debug_str = format!("{:?}", node);
        // Extract just the fields part (after the variant name)
        let fields_str = if let Some(pos) = debug_str.find('{') {
          &debug_str[pos..]
        } else {
          &debug_str
        };
        output.push_str(&format!("{}{}{}\n", indent, node_name, fields_str));
        
        // For nodes that contain ASTIndex children, we need to recursively process them
        // This is a fallback for nodes we haven't explicitly handled
        self.print_ast_children_debug(node, indent_level + 1, output);
      }
    }
  }

  /// Helper to recursively process ASTIndex children for nodes that weren't explicitly handled
  fn print_ast_children_debug(&self, node: &ASTNode, indent_level: usize, output: &mut String) {
    use ASTNode::*;
    match node {
      Block { nodes } => {
        for child_index in nodes {
          self.print_ast_node_debug(*child_index, indent_level, output);
        }
      }
      Parenthesis { nodes } => {
        for child_index in nodes {
          self.print_ast_node_debug(*child_index, indent_level, output);
        }
      }
      Array { nodes } => {
        for child_index in nodes {
          self.print_ast_node_debug(*child_index, indent_level, output);
        }
      }
      Dict { properties } => {
        for property in properties {
          self.print_object_property_debug(property, indent_level, output);
        }
      }
      // Add more cases as needed for other node types with children
      _ => {}
    }
  }

  fn print_destructurable_declaration_debug(
    &self,
    declaration: &DestructurableDeclaration,
    indent_level: usize,
    output: &mut String
  ) {
    let indent = "  ".repeat(indent_level);
    output.push_str(&format!("{}DestructurableDeclaration\n", indent));
    
    let pattern_indent = "  ".repeat(indent_level + 1);
    output.push_str(&format!("{}Pattern:\n", pattern_indent));
    self.print_destructure_pattern_debug(&declaration.name, indent_level + 2, output);
    
    output.push_str(&format!("{}Type: {:?}\n", pattern_indent, declaration.typ));
  }

  fn print_destructure_pattern_debug(
    &self,
    pattern: &DestructurePattern,
    indent_level: usize,
    output: &mut String
  ) {
    let indent = "  ".repeat(indent_level);
    
    match pattern {
      DestructurePattern::Array { elements, spread } => {
        output.push_str(&format!("{}ArrayPattern ({} elements)\n", indent, elements.len()));
        for element in elements {
          self.print_destructure_pattern_debug(element, indent_level + 1, output);
        }
        if let Some(spread_pattern) = spread {
          output.push_str(&format!("{}Spread:\n", indent));
          self.print_destructure_pattern_debug(spread_pattern, indent_level + 1, output);
        }
      }
      DestructurePattern::Object { properties, spread } => {
        output.push_str(&format!("{}ObjectPattern ({} properties)\n", indent, properties.len()));
        for (key, value) in properties {
          output.push_str(&format!("{}  Property:\n", indent));
          output.push_str(&format!("{}    Key:\n", indent));
          self.print_destructure_pattern_debug(key, indent_level + 2, output);
          output.push_str(&format!("{}    Value:\n", indent));
          self.print_destructure_pattern_debug(value, indent_level + 2, output);
        }
        if let Some(spread_pattern) = spread {
          output.push_str(&format!("{}Spread:\n", indent));
          self.print_destructure_pattern_debug(spread_pattern, indent_level + 1, output);
        }
      }
      DestructurePattern::Identifier { name } => {
        output.push_str(&format!("{}Identifier: {}\n", indent, self.str_src(*name)));
      }
      DestructurePattern::NumericProperty { value } => {
        output.push_str(&format!("{}NumericProperty: {}\n", indent, self.str_src(*value)));
      }
      DestructurePattern::StringProperty { value } => {
        output.push_str(&format!("{}StringProperty: {}\n", indent, self.str_src(*value)));
      }
      DestructurePattern::Ignore => {
        output.push_str(&format!("{}Ignore\n", indent));
      }
      DestructurePattern::WithInitializer { pattern, initializer } => {
        output.push_str(&format!("{}WithInitializer\n", indent));
        output.push_str(&format!("{}  Pattern:\n", indent));
        self.print_destructure_pattern_debug(pattern, indent_level + 2, output);
        output.push_str(&format!("{}  Initializer:\n", indent));
        self.print_ast_node_debug(*initializer, indent_level + 2, output);
      }
    }
  }

  fn print_class_member_debug(&self, member: &ClassMember, indent_level: usize, output: &mut String) {
    let indent = "  ".repeat(indent_level);
    match member {
      ClassMember::Property(declaration, modifiers) => {
        let modifiers_str = if modifiers.is_empty() { "" } else { &modifiers.emit(true) };
        output.push_str(&format!("{}Property {}\n", indent, modifiers_str));
      }
      ClassMember::Method(method, getter_setter) => {
        let getter_setter_str = getter_setter.as_str();
        let name = method.name.map(|n| self.str_src(n)).unwrap_or("<anonymous>");
        output.push_str(&format!("{}Method {}{}\n", indent, getter_setter_str, 
          if getter_setter_str.is_empty() { "" } else { " " }));
        output.push_str(&format!("{}  Name: {}\n", indent, name));
      }
      ClassMember::StaticBlock(body) => {
        output.push_str(&format!("{}StaticBlock\n", indent));
        output.push_str(&format!("{}  Body:\n", indent));
        self.print_ast_node_debug(*body, indent_level + 2, output);
      }
    }
  }

  fn print_object_property_debug(&self, property: &ObjectProperty, indent_level: usize, output: &mut String) {
    let indent = "  ".repeat(indent_level);
    match property {
      ObjectProperty::Property { computed, key, value } => {
        output.push_str(&format!("{}Property (computed: {})\n", indent, computed));
        output.push_str(&format!("{}  Key:\n", indent));
        self.print_ast_node_debug(*key, indent_level + 2, output);
        output.push_str(&format!("{}  Value:\n", indent));
        self.print_ast_node_debug(*value, indent_level + 2, output);
      }
      ObjectProperty::Rest { argument } => {
        output.push_str(&format!("{}RestProperty\n", indent));
        output.push_str(&format!("{}  Argument:\n", indent));
        self.print_ast_node_debug(*argument, indent_level + 2, output);
      }
      ObjectProperty::Shorthand { key } => {
        output.push_str(&format!("{}ShorthandProperty: {}\n", indent, self.str_src(*key)));
      }
    }
  }
}
