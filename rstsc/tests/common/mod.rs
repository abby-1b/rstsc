use std::{process::{Command, Stdio}};

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

use reqwest::blocking::Client;

const API_ENDPOINT: &str = "http://localhost:8033/compile";
const API_ENDPOINT_START: &str = "http://localhost:8033/reset_counter";
const API_ENDPOINT_END: &str = "http://localhost:8033/print_counter";

pub fn tsc_compile_snippets(ts_snippets: &Vec<&str>) -> Vec<(String, bool)> {
	let client = Client::new();

	// Start counting (server-side)
	let _ = client
		.post(API_ENDPOINT_START)
		.send();
		
	let out = ts_snippets.iter().map(|ts_code| {
		// Send a POST request to the server with the TypeScript code in the body.
		let snippet = (*ts_code).to_owned();
		let response_res = client
			.post(API_ENDPOINT)
			.header("Content-Type", "text/plain")
			.body(snippet)
			.send();
		let response = match response_res {
			Err(err) => return (format!("{:?}", err), false),
			Ok(res) => res
		};
	
		// Check if the request was successful
		if response.status().is_success() {
			// If successful, return the response body as a string.
			match response.text() {
				Err(err) => return (format!("{:?}", err), false),
				Ok(text) => (text, true)
			}
		} else {
			// If not successful, create an error with the status and response text.
			let error_message = format!(
				"API request failed with status {}: {}",
				response.status(),
				response.text().unwrap_or_else(|_| "No details".to_string())
			);
			(error_message, false)
		}
	}).collect();

	// Finish counting (server-side)
	let _ = client
		.post(API_ENDPOINT_END)
		.send();

	return out;
}

/// Tests TypeScript code, checking the passed in TSC output with RSTSC's.
/// Ignores leading and trailing whitespace and semicolons.
pub fn test_code(source: &str, compiled: &str, whitespace: &WhiteSpace) -> Result<(), String> {
	let mut code_string = source.to_string();
	code_string += "\n ";
	let code = code_string.as_str();

	// Get the `rstsc` output
	// println!("RSTSC compiling: {:?}", code);
	let out = std::panic::catch_unwind(|| {
		rstsc::compile(code)
	});
 	let mut actual: String = match out {
		Ok(Ok(code)) => code.clone(),
		Ok(Err(err)) => format!("{:?}", err),
		Err(err) => format!("{:?}", err)
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

	// if out.is_ok() { return Ok(()) }

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
