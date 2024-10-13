/// The main CLI tool for RSTSC!
/// Use this to compile individual files.
/// Eventually, this will be able to compile whole projects!

pub fn main() {
  let args: Vec<String> = std::env::args().collect();
  if args.len() < 2 { print_help(); }
  dbg!(args);
}

fn print_help() {
  print_version();
}

fn print_version() {
  println!("RSTSC v{}", env!("CARGO_PKG_VERSION"));
}
