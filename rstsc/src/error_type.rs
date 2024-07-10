use crate::tokenizer::Token;

pub struct CompilerError<'a> {
    pub message: String,
    pub token: Token<'a>,
}

impl<'a> std::fmt::Debug for CompilerError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.message)?;
        Ok(())
    }
}

impl<'a> CompilerError<'a> {
    // Throws this error, exiting the program.
    pub fn throw(&self) {
        println!("\n\x1b[31m{:?}\x1b[0m", self);
        std::process::exit(1);
    }
}
