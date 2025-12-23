mod common;
use crate::common::{Tester, WhiteSpace};

const SOURCE: &str = include_str!("./tests.ts");

#[test]
fn tsc_tests() {
  let source_snippets: Vec<&str> = SOURCE.split("\n\n").collect();
  let mut tester = Tester::new();
  tester.test_many(WhiteSpace::IgnoreAll, &source_snippets);
  tester.finish();
}
