use common::{test_code, WhiteSpace};
mod common;

#[test]
fn declare_single() {
	test_code(
		"declare some {}",
		" ",
		WhiteSpace::Ignore, true
	);
	test_code(
		"declare global { let some: number = 123; }",
		" ",
		WhiteSpace::Ignore, true
	);
	test_code(
		"declare global {
			interface Window {
				presentation: Presentation;
				slide: Slide;
			}
			let aPrime: someType = 123;
		}",
		" ",
		WhiteSpace::Ignore, true
	);
}

#[test]
fn declare_others() {
	test_code(
		"declare const someVar = 123;",
		" ",
		WhiteSpace::Ignore, true
	);
	test_code(
		"declare const someOtherVar: number = 1234;",
		" ",
		WhiteSpace::Ignore, true
	);
	test_code(
		"declare class SomeClass {}",
		" ",
		WhiteSpace::Ignore, true
	);
	test_code(
		"declare namespace Some.const {}",
		" ",
		WhiteSpace::Ignore, true
	);
}
