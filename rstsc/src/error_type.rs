use crate::tokenizer::{Token, TokenList};

pub struct CompilerError<'a> {
    pub message: String,
    pub token: Token<'a>,
}

impl<'a> std::fmt::Debug for CompilerError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}\n", &self.message)?;
        write!(f, "{:?}", self.token)?;
        Ok(())
    }
}

impl<'a> CompilerError<'a> {
    // Throws this error, exiting the program.
    pub fn throw(&self, tokens: TokenList) {
        let _ = tokens;
        // let token_ptr = (&self.token.value.as_bytes()[0]) as *const u8;

        // // Get the line number of the error token
        // let mut line_num = 0;
        // for c in tokens.source.as_bytes() {
        //     println!(
        //         "line {}  char {}  ptr {:?}  target {:?}",
        //         line_num, c,
        //         c as *const u8, token_ptr
        //     );
        //     if *c == '\n' as u8 {
        //         line_num += 1;
        //     }
        //     if c as *const u8 > token_ptr {
        //         break
        //     }
        // }

        println!(
            "\n\x1b[31m{:?}\nLine: {}\x1b[0m",
            self.message,
            // line_num
            0
        );
        std::process::exit(1);
    }

    pub fn expected(
        expect: &str,
        token: Token<'a>
    ) -> CompilerError<'a> {
        CompilerError {
            message: format!(
                "Expected {:?}, found {:?}",
                expect,
                token.value
            ),
            token
        }
    }
}
