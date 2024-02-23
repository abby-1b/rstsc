use regex::Regex;

const TEST_PRE: &str = "let a:number=0;\n";
const TEST_POST: &str = "\nlet aa:number=0;";

const EXPECT_PRE: &str = "let a=0;\n";
const EXPECT_POST: &str = "\nlet aa=0;";

pub enum WhiteSpace {
	/// Ignores all whitespace
	Ignore,
	
	/// Makes sure that there is whitespace between characters, but
	/// doesn't care about how MUCH whitespace there is
	HasWhitespace,

	/// Matches both strings lists EXACTLY
	Exact,
}

/// Tests TypeScript code, completely ignoring whitespace
/// Leave "expected" empty (`""`) if you expect an error.
pub fn test_code(
	code_str: &str, expected_str: &str,
	whitespace: WhiteSpace,
	semicolons_are_whitespace: bool
) {
	let input_code = TEST_PRE.to_owned() + code_str + TEST_POST;
	let out = ts_compile::compile(input_code.as_str());

	// Out being empty means error!
	if expected_str.len() == 0 && out.is_err() { return; }

	// Make these into the right types, and add beginning and ending padding
	// to make sure the compiler doesn't cut anything off and/or stop
	// transforming after a test (due to mismanaged state)
	let mut actual: String = out.unwrap();
	let actual_untransformed = actual.clone();
	let mut expect: String = EXPECT_PRE.to_string() + &expected_str.to_string() + EXPECT_POST;
	let expect_untransformed = expect.clone();

	if semicolons_are_whitespace {
		actual = actual.replace(';', " ");
		expect = expect.replace(';', " ");
	}

	// .replace(char::is_alphabetic, "A");
	match whitespace {
		WhiteSpace::Exact => {},
		WhiteSpace::Ignore => {
			let re = Regex::new(r"[\n\t ]").unwrap();
			actual = re.replace_all(actual.as_str(), "").to_string();
			expect = re.replace_all(expect.as_str(), "").to_string();
		},
		WhiteSpace::HasWhitespace => {
			let re = Regex::new(r"[\n\t ]+").unwrap();
			actual = re.replace_all(actual.as_str(), " ").to_string();
			expect = re.replace_all(expect.as_str(), " ").to_string();
		}
	};

	if actual != expect {
		panic!(
			"Input code:\x1b[32m\n{}\x1b[0m\nExpected output:\n\x1b[33m{}\x1b[0m\nGot output:\n\x1b[31m{}\x1b[0m",
			input_code,
			expect_untransformed,
			actual_untransformed,
		);
	}
}
