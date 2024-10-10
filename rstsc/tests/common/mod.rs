use std::{io::Read, process::{Command, Stdio}};

use rand::Rng;
use regex::Regex;

pub enum WhiteSpace {
	/// Ignores all whitespace. Semicolons and parenthesis
	/// are treated as whitespace.
	IgnoreAll,
	
	/// Makes sure that there is whitespace between characters,
	/// but doesn't care about how much whitespace there is.
	/// Semicolons are treated as whitespace.
	HasWhitespace,

	/// Matches both strings lists exactly
	Exact,
}

/// Compiles TypeScript using `tsc`
pub fn tsc_compile(code: &str) -> Result<String, String> {
	// Get a random filename
	let mut filename = rand::thread_rng().gen::<f32>().to_string();
	filename += "test";
	let filename_ts = filename.clone() + ".ts";
	let filename_js = filename + ".js";

	// Write the file
	std::fs::write(&filename_ts, code).unwrap();

	let child = Command::new("tsc")
        .arg("--target")
        .arg("ESNext")
        .arg(&filename_ts)
        .arg("--outFile")
        .arg(&filename_js)
		.stdout(Stdio::null())
        .spawn();

	// Wait for completion
	let mut child = child.expect("`tsc` not found!");
	child.wait().expect("`tsc` wasn't running!");
	
	let out = if let Ok(out) = std::fs::read_to_string(&filename_js) {
		Ok(out)
	} else {
		let mut err = "Error: ".to_string();
		if let Some(mut stderr) = child.stderr { let _ = stderr.read_to_string(&mut err); }
		if let Some(mut stdout) = child.stdout { let _ = stdout.read_to_string(&mut err); }
		Err(err)
	};

	// Delete the files
	std::fs::remove_file(&filename_ts).unwrap();
	std::fs::remove_file(&filename_js).unwrap();

	out
}

/// Tests TypeScript code, compiling it with TSC and checking the two strings.
/// Ignores leading and trailing whitespace and semicolons.
pub fn test_code<'a>(source: &str, compiled: &str, whitespace: &WhiteSpace) -> Result<(), String> {
	let mut code_string = source.to_string();
	code_string += "\n ";
	let code = code_string.as_str();

	// Get the `rstsc` output
	// println!("RSTSC compiling: {:?}", code);
	let out = rstsc::compile(code);
	let mut actual: String = if let Ok(out) = out {
		out
	} else {
		format!("{:?}", out.err().unwrap())
	};
	let actual_untransformed: String = actual.clone();

	// Get the `tsc` output
	let mut expect: String = compiled.to_owned();
	let expect_untransformed: String = expect.clone();

	actual = actual.trim_matches(|c| "\n\t; ".contains(c)).to_string();
	expect = expect.trim_matches(|c| "\n\t; ".contains(c)).to_string();

	match whitespace {
		WhiteSpace::IgnoreAll => {
			let re = Regex::new(r"[\n\t;,() ]+").unwrap();
			actual = re.replace_all(actual.as_str(), "").to_string();
			expect = re.replace_all(expect.as_str(), "").to_string();
		}
		WhiteSpace::HasWhitespace => {
			let re = Regex::new(r"[\n\t;() ]+").unwrap();
			actual = re.replace_all(actual.as_str(), " ").to_string();
			expect = re.replace_all(expect.as_str(), " ").to_string();
		}
		WhiteSpace::Exact => {}
	}

	if actual != expect {
		Err(format!(
			concat!(
				"Input code:\x1b[32m\n{}\x1b[0m\n",
				"Expected output:\n\x1b[33m{}\x1b[0m\n",
				"Got output:\n\x1b[31m{}\x1b[0m\n",
				"----------------------------------------"
			),
			code,
			expect_untransformed,
			actual_untransformed,
		))
	} else {
		Ok(())
	}
}

// /// Tests multiple code snippets at once
// pub fn test_multiple(
// 	input_snippets: &[&str],
// 	whitespace: WhiteSpace
// ) {
// 	let mut fails = vec![];

// 	let mut count_failed: usize = 0;
// 	let mut count_correct: usize = 0;

// 	for snippet in input_snippets {
// 		if let Err(fail_message) = test_code(snippet, &whitespace) {
// 			fails.push(fail_message);
// 			count_failed += 1;
// 		} else {
// 			count_correct += 1;
// 		}
// 	}

// 	if !fails.is_empty() {
// 		// Some fails
// 		for fail in fails {
// 			println!("{}", fail);
// 		}

// 		panic!(
// 			"{} correct, {} failed ({:.2}% passed)",
// 			count_correct,
// 			count_failed,
// 			(count_correct as f64 / (count_correct + count_failed) as f64) * 100.0
// 		);
// 	}
// }
