use common::{test_code, WhiteSpace};
mod common;

#[test]
fn primitive_types() {
	test_code(
		"const a: number = 1;",
		"const a = 1;",
		WhiteSpace::Ignore, true
	);
	test_code(
		"const a: string = '1';",
		"const a = '1';",
		WhiteSpace::Ignore, true
	);
	test_code(
		"const a: boolean = true;",
		"const a = true;",
		WhiteSpace::Ignore, true
	);
	test_code(
		"const a: bigint = 0n;",
		"const a = 0n;",
		WhiteSpace::Ignore, true
	);
}

#[test]
fn primitive_arrays() {
	test_code(
		"const a: number[] = [1, 2, 3];",
		"const a = [1, 2, 3];",
		WhiteSpace::Ignore, true
	);
	test_code(
		"const a: string[] = ['1', '2', '3'];",
		"const a = ['1', '2', '3'];",
		WhiteSpace::Ignore, true
	);
	test_code(
		"const a: boolean[] = [true, false, true];",
		"const a = [true, false, true];",
		WhiteSpace::Ignore, true
	);
	test_code(
		"const a: bigint[] = [0n, 1n, 2n];",
		"const a = [0n, 1n, 2n];",
		WhiteSpace::Ignore, true
	);
}

#[test]
fn primitive_same_type_tuples() {
	test_code(
		"const a: [number, number, number] = [1, 2, 3];",
		"const a = [1, 2, 3];",
		WhiteSpace::Ignore, true
	);
	test_code(
		"const a: [string, string, string] = ['1', '2', '3'];",
		"const a = ['1', '2', '3'];",
		WhiteSpace::Ignore, true
	);
	test_code(
		"const a: [boolean, boolean, boolean] = [true, false, true];",
		"const a = [true, false, true];",
		WhiteSpace::Ignore, true
	);
	test_code(
		"const a: [bigint, bigint, bigint] = [0n, 1n, 2n];",
		"const a = [0n, 1n, 2n];",
		WhiteSpace::Ignore, true
	);
}

#[test]
fn primitive_multi_type_tuples() {
	test_code(
		"const a: [number, boolean, string] = [1, false, '3'];",
		"const a = [1, false, '3'];",
		WhiteSpace::Ignore, true
	);
	test_code(
		"const a: [string, bigint, number] = ['1', 2n, 3];",
		"const a = ['1', 2n, 3];",
		WhiteSpace::Ignore, true
	);
}

#[test]
fn primitive_arrays_with_generics() {
	test_code(
		"const a: Array<number> = [1, 2, 3];",
		"const a = [1, 2, 3];",
		WhiteSpace::Ignore, true
	);
	test_code(
		"const a: Array<string> = ['1', '2', '3'];",
		"const a = ['1', '2', '3'];",
		WhiteSpace::Ignore, true
	);
	test_code(
		"const a: Array<boolean> = [true, false, true];",
		"const a = [true, false, true];",
		WhiteSpace::Ignore, true
	);
	test_code(
		"const a: Array<bigint> = [0n, 1n, 2n];",
		"const a = [0n, 1n, 2n];",
		WhiteSpace::Ignore, true
	);
}

#[test]
fn custom_types() {
	test_code(
		"const a: MyClass = new MyClass();",
		"const a = new MyClass();",
		WhiteSpace::Ignore, true
	);
	test_code(
		"const a: typeof MyClass = MyClass;",
		"const a = MyClass;",
		WhiteSpace::Ignore, true
	);
}

#[test]
fn function_types() {
	test_code(
		"let a: () => void;",
		"let a;",
		WhiteSpace::Ignore, true
	);
	test_code(
		"let a: () => number = () => 123;",
		"let a = () => 123;",
		WhiteSpace::Ignore, true
	);
}
