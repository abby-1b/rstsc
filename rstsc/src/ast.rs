
use core::fmt::Debug;

use crate::types::Type;

#[derive(Debug, Clone, PartialEq)]
pub enum VariableDefType {
    Var,
    Let,
    Const
}
impl VariableDefType {
    pub fn emit(&self) -> String {
        match self {
            VariableDefType::Var => "var ",
            VariableDefType::Let => "let ",
            VariableDefType::Const => "const ",
        }.to_string()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ObjectProperty {
    Property {
        computed: bool,
        key: ASTNode,
        value: ASTNode
    },
    Spread {
        argument: ASTNode
    }
}

#[derive(Default, Clone, PartialEq)]
/// A list of modifier flags
pub struct ModifierList {
    /// A list of bit flags (lowest to highest) detailing:
    ///  - export
    ///  - async
    ///  - static
    ///  - public
    ///  - private
    ///  - protected
    pub flags: u8,
}

pub const MODIFIERS: &[&str] = &[
    "export",
    "async",
    "public", "private", "protected",
    "static"
];
pub const MODIFIER_IS_JS: &[bool] = &[
    true,
    true,
    false, false, false,
    true
];

#[derive(Clone)]
/// A single modifier
pub enum Modifier {
    Export,
    Async,
    Static,
    Public,
    Private,
    Protected,
}
impl Modifier {
    pub fn as_flag(&self) -> u8 {
        match self {
            Modifier::Export => 1 << 0,
            Modifier::Async => 1 << 1,
            Modifier::Static => 1 << 2,
            Modifier::Public => 1 << 3,
            Modifier::Private => 1 << 4,
            Modifier::Protected => 1 << 5,
        }
    }
}

impl ModifierList {
    /// Sets the modifier in the list
    pub fn set(&mut self, modifier: Modifier) {
        self.flags |= modifier.as_flag();
    }

    /// Checks if the modifier exists within the list
    pub fn has(&mut self, modifier: Modifier) -> bool {
        self.flags & modifier.as_flag() != 0
    }

    pub fn emit(&self, js_only: bool) -> String {
        let mut out = String::new();
        for idx in 0..6 {
            let flag = 1 << idx;
            if self.flags & flag == 0 || (js_only && !MODIFIER_IS_JS[idx]) {
                continue;
            }
            out += MODIFIERS[idx];
            out += " ";
        }
        out
    }
}

impl Debug for ModifierList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(
                (0..6).filter_map(|idx| {
                    let flag = 1 << idx;
                    if self.flags & flag== 0 {
                        return None;
                    }
                    Some(MODIFIERS[idx])
                })
            )
            .finish()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NamedDeclaration {
    pub name: String,
    pub typ: Option<Type>,
    pub value: Option<ASTNode>,

    /// True when this declaration is conditional
    /// (eg. `someVar?: string`)
    pub conditional: bool,

    /// True when this declaration starts with a spread
    /// (eg. `(...args)`)
    pub spread: bool
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTNode {
    /// A block of code
    Block { nodes: Vec<ASTNode> },

    /// A declaration that contains a name and type.
    /// Can also contain an optional value
    Declaration {
        on: Box<ASTNode>,
        typ: Type,

        // Whether or not this declaration has the `?` sign before its type
        conditional: bool
    },

    /// A `var`, `let` or `const` variable definition
    VariableDeclaration {
        modifiers: ModifierList,
        def_type: VariableDefType,
        defs: Vec<NamedDeclaration>
    },

    StatementIf {
        condition: Box<ASTNode>,
        body: Box<ASTNode>,
        alternate: Option<Box<ASTNode>>
    },
    StatementWhile {
        condition: Box<ASTNode>,
        body: Box<ASTNode>
    },
    StatementFor {
        init: Box<ASTNode>,
        condition: Box<ASTNode>,
        update: Box<ASTNode>,
        body: Box<ASTNode>
    },

    StatementReturn { value: Option<Box<ASTNode>> },
    StatementBreak { value: Option<Box<ASTNode>> },
    StatementContinue { value: Option<Box<ASTNode>> },
    StatementThrow { value: Option<Box<ASTNode>> },

    FunctionDefinition {
        modifiers: ModifierList,
        name: Option<String>,
        generics: Option<Vec<(Type, Option<Type>)>>,
        params: Vec<NamedDeclaration>,
        return_type: Option<Type>,
        body: Option<Box<ASTNode>>
    },

    ArrowFunctionDefinition {
        params: Vec<NamedDeclaration>,
        return_type: Option<Type>,
        body: Box<ASTNode>
    },

    // Used for expressions

    Parenthesis { nodes: Vec<ASTNode> },
    Array { nodes: Vec<ASTNode> },
    Dict { properties: Vec<ObjectProperty> },

    // Expression parsing...

    ExprIdentifier { name: String },
    ExprNumLiteral { number: String },
    ExprStrLiteral { string: String },
    ExprBoolLiteral { value: bool },

    ExprFunctionCall { callee: Box<ASTNode>, arguments: Vec<ASTNode> },
    ExprIndexing { callee: Box<ASTNode>, property: Vec<ASTNode> },

    ExprTernary {
        condition: Box<ASTNode>,
        if_true: Box<ASTNode>,
        if_false: Box<ASTNode>
    },

    PrefixOpr { opr: String, expr: Box<ASTNode> },
    InfixOpr { left: Box<ASTNode>, opr: String, right: Box<ASTNode> },
    PostfixOpr { expr: Box<ASTNode>, opr: String },

    // Type casting

    /// Casting something with the `as` keyword (eg. `obj as any`)
    ExprAs {
        value: Box<ASTNode>,
        cast_type: Type
    },

    /// Casting something C-style (eg. `<any>obj`)
    ExprTypeAssertion {
        cast_type: Type,
        value: Box<ASTNode>,
    },

    TypeDeclaration {
        first_typ: Type,
        equals_typ: Type
    },

    /// Used in situations like `[ 1, 2, ]` where there's an empty expression
    Empty
}

impl ASTNode {
    /// Gets the name of this enum, without the associated data
    pub fn name(&self) -> String {
        match self {
            ASTNode::Block { .. } => "Block",
            ASTNode::Declaration { .. } => "Declaration",
            ASTNode::VariableDeclaration { .. } => "VariableDeclaration",
            ASTNode::StatementIf { .. } => "StatementIf",
            ASTNode::StatementWhile { .. } => "StatementWhile",
            ASTNode::StatementFor { .. } => "StatementFor",
            ASTNode::StatementReturn { .. } => "StatementReturn",
            ASTNode::StatementBreak { .. } => "StatementBreak",
            ASTNode::StatementContinue { .. } => "StatementContinue",
            ASTNode::StatementThrow { .. } => "StatementThrow",
            ASTNode::FunctionDefinition { .. } => "FunctionDefinition",
            ASTNode::ArrowFunctionDefinition { .. } => "ArrowFunctionDefinition",
            ASTNode::Parenthesis { .. } => "Parenthesis",
            ASTNode::Array { .. } => "Array",
            ASTNode::Dict { .. } => "Dict",
            ASTNode::ExprNumLiteral { .. } => "ExprNumLiteral",
            ASTNode::ExprStrLiteral { .. } => "ExprStrLiteral",
            ASTNode::ExprIdentifier { .. } => "ExprIdentifier",
            ASTNode::ExprBoolLiteral { .. } => "ExprBoolLiteral",
            ASTNode::ExprFunctionCall { .. } => "ExprFunctionCall",
            ASTNode::ExprIndexing { .. } => "ExprIndexing",
            ASTNode::ExprTernary { .. } => "ExprTernary",
            ASTNode::PrefixOpr { .. } => "PrefixOpr",
            ASTNode::InfixOpr { .. } => "InfixOpr",
            ASTNode::PostfixOpr { .. } => "PostfixOpr",
            ASTNode::ExprAs { .. } => "ExprAs",
            ASTNode::ExprTypeAssertion { .. } => "ExprTypeAssertion",
            ASTNode::TypeDeclaration { .. } => "TypeDeclaration",
            ASTNode::Empty { .. } => "Empty",
        }.to_string()
    }

    /// Checks if a node is a literal
    pub fn is_literal(&self) -> bool {
        match self {
            ASTNode::ExprNumLiteral { .. } => true,
            ASTNode::ExprStrLiteral { .. } => true,
            ASTNode::ExprBoolLiteral { .. } => true,
            _ => false
        }
    }

    pub fn apply_modifiers(&mut self, new_modifiers: ModifierList) -> Result<(), String> {
        match self {
            ASTNode::VariableDeclaration { modifiers, .. } => {
                *modifiers = new_modifiers;
                Ok(())
            },
            ASTNode::FunctionDefinition { modifiers, .. } => {
                *modifiers = new_modifiers;
                Ok(())
            },
            _ => {
                Err(format!(
                    "{} can't have modifiers!",
                    self.name()
                ))
            }
        }
    }
}
