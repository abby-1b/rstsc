use common::{test_code, WhiteSpace};
mod common;

#[test]
fn generic_single_arg() {
	test_code(
		"const a: SomeType<number> = 1;",
		"const a = 1;",
		WhiteSpace::Ignore, true
	);
	test_code(
		"const a: SomeType<string> = 'hey';",
		"const a = 'hey';",
		WhiteSpace::Ignore, true
	);
	test_code(
		"const a: SomeType<[string, number]> = ['hey', 1];",
		"const a = ['hey', 1];",
		WhiteSpace::Ignore, true
	);
	test_code(
		"const a: SomeType<() => void> = () => { 'amazing' };",
		"const a = () => { 'amazing' };",
		WhiteSpace::Ignore, true
	);
}

#[test]
fn generic_double_arg() {
	test_code(
		"const a: Record<string, number> = {'a':1,'b':2};",
		"const a = {'a':1,'b':2};",
		WhiteSpace::Ignore, true
	);
	test_code(
		"const a: Map<number, string> = new Map();",
		"const a = new Map();",
		WhiteSpace::Ignore, true
	);
	test_code(
		"const a: SomeType<() => void, [string, number]> = undefined;",
		"const a = undefined;",
		WhiteSpace::Ignore, true
	);
}

#[test]
fn generic_nested() {
	test_code(
		"const a: Record<string, Record<number, string>> = {};",
		"const a = {};",
		WhiteSpace::Ignore, true
	);
}
