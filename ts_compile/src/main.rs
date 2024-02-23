#![feature(linked_list_cursors)]

mod compiler;
pub mod server;
pub mod tokenizer;
pub mod asi;
pub mod transformer;
pub mod emitter;

use server::start_server;

fn main() {
    // let output = compiler::compile("
    // const a: () => void = 123;
    // ");
    // if output.is_err() {
    //     println!("Error: {}", output.err().unwrap());
    // } else {
    //     println!("{}", output.unwrap());
    // }

    start_server();
}
