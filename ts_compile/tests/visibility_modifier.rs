use common::{test_code, WhiteSpace};
mod common;

#[test]
fn test_public() {
	test_code(
		"class A { public a = 0; }",
		"class A { a = 0; }",
		WhiteSpace::Ignore, true
	);
	test_code(
		"class A { public method() {} }",
		"class A { method() {} }",
		WhiteSpace::Ignore, true
	);
	// test_code(
	// 	"class A { constructor(public a: string) {} }",
	// 	"class A { constructor(a) { this.a = a; } }",
	// 	WhiteSpace::Ignore, true
	// );
}

#[test]
fn test_private() {
	test_code(
		"class A { private a = 0; }",
		"class A { a = 0; }",
		WhiteSpace::Ignore, true
	);
	test_code(
		"class A { private method() {} }",
		"class A { method() {} }",
		WhiteSpace::Ignore, true
	);
	// test_code(
	// 	"class A { constructor(private a: string) {} }",
	// 	"class A { constructor(a) { this.a = a; } }",
	// 	WhiteSpace::Ignore, true
	// );
}

#[test]
fn test_protected() {
	test_code(
		"class A { protected a = 0; }",
		"class A { a = 0; }",
		WhiteSpace::Ignore, true
	);
	test_code(
		"class A { protected method() {} }",
		"class A { method() {} }",
		WhiteSpace::Ignore, true
	);
	// test_code(
	// 	"class A { constructor(protected a: string) {} }",
	// 	"class A { constructor(a) { this.a = a; } }",
	// 	WhiteSpace::Ignore, true
	// );
}

#[test]
fn test_abstract() {
	test_code(
		"abstract class A {}",
		"class A {}",
		WhiteSpace::Ignore, true
	);
}
