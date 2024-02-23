use common::{test_code, WhiteSpace};
mod common;

#[test]
fn normal_functions() {
	test_code(
		"function foo(a: number, b: number) { return a + b; }",
		"function foo(a, b) { return a + b; }",
		WhiteSpace::Ignore, true
	);
	test_code(
		"function foo(a: number, b: number) { return a + b; }",
		"function foo(a, b) { return a + b; }",
		WhiteSpace::Ignore, true
	);
}
