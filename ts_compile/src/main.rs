#![feature(linked_list_cursors)]

mod compiler;
pub mod tokenizer;
pub mod asi;
pub mod transformer;
pub mod emitter;

// const SOURCE: &str = include_str!("../test.ts");

// const a = (hey: number): number => return 123;
// const b = true ? undefined : 'some str';
// const c = b?.split(' ')

const SOURCE: &str = "
let a = {
    'key': 'value'
};
";

fn main() {
    let output = compiler::compile(SOURCE);
    if output.is_err() {
        println!("Error: {}", output.err().unwrap());
    } else {
        println!("{}", output.unwrap());
    }
}
