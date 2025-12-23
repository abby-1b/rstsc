use std::{collections::HashMap, process::{Command, Stdio}};

use lazy_static::lazy_static;
use regex::Regex;
use reqwest::blocking::Client;

const API_ENDPOINT: &str = "http://localhost:8033/compile";
const API_ENDPOINT_START: &str = "http://localhost:8033/reset_counter";
const API_ENDPOINT_END: &str = "http://localhost:8033/print_counter";

const REQUIRED_TESTS: usize = 4;

#[derive(Clone)]
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

pub struct Tester {
  fails: Vec<String>,
  count_correct: usize,
  count_failed: usize,
	tags: Vec<(&'static str, Vec<Regex>)>,
  tag_counts: HashMap<&'static str, [usize; 2]>,
}

impl Tester {
  pub fn new() -> Self
  {
    let pre_tags: Vec<(&str, &[&str])> = vec![
      ("Function", &[ r"function " ]),
      ("Variable", &[ r"const ", r"let ", r"var " ]),
      ("Type Decl", &[ r"[^a-zA-Z]type " ]),
      ("Interface", &[ r"[^a-zA-Z]interface " ]),
      ("Class", &[ r"[^a-zA-Z]class( |<)" ]),
      ("Enum", &[ r"enum " ]),
      ("Generic", &[ r"<[a-zA-Z]*?>" ]),
      ("Array", &[ r"\[", r"\]" ]),
      ("T-Condition", &[ r" extends .*? :" ]),
      ("T-Key", &[ r"\[( |)key( |):[ a-zA-Z]*?\]:" ]),
      ("Arrow Fn", &[ r"=>" ]),
      ("Rest/Spread", &[ r"\.\.\." ]),
      ("Getter", &[ r"get " ]),
      ("Setter", &[ r"set " ]),
      ("A-Params", &[ r"\(\s*public|\(\s*private" ]),
      ("NN-Assert", &[ r"!;", r"!\." ]),
      ("Opt-Chaining", &[ r"\?\." ]),
      ("Opt-Params", &[ r"\?: " ]),
      ("Import", &[ r"[^a-zA-Z]import " ]),
      ("Export", &[ r"[^a-zA-Z]export " ]),
      ("Namespace", &[ r"[^a-zA-Z]namespace " ]),
      ("Try/Catch", &[ r"[^a-zA-Z]try " ]),
      ("Decorator", &[ r"@" ]),
      ("Tuple", &[ r"\[.*?,.*?\]", r"\[.*?,.*?,.*?\]", r"\[.*?,.*?,.*?,.*?\]" ]),
      ("Literal Types", &[ r#": (true|false|[0-9]+|'.*?'|".*?")"# ]),
      ("Async", &[ r"[^a-zA-Z]async ", r"[^a-zA-Z]await " ]),
      ("Comments", &[ r"//", r"/\*", r"\*/" ]),
      ("Extends", &[ r"[^a-zA-Z]extends " ]),
      ("Implements", &[ r"[^a-zA-Z]implements " ]),
      ("Abstract", &[ r"[^a-zA-Z]abstract " ]),
      ("Static", &[ r"[^a-zA-Z]static " ]),
      ("Readonly", &[ r"[^a-zA-Z]readonly " ]),
      ("Constructor", &[ r"[^a-zA-Z]constructor\(" ]),
      ("Super", &[ r"[^a-zA-Z]super\(", r"[^a-zA-Z]super\." ]),
      ("This", &[ r"[^a-zA-Z]this\." ]),
    ];

    // Compile tag regexes
    let mut tags: Vec<(&str, Vec<Regex>)> = vec![];
    for pre_tag in pre_tags {
      tags.push((
        pre_tag.0,
        pre_tag.1.iter().map(|s| Regex::new(s).unwrap()).collect()
      ));
    }

    // Add tags to hashmap
    let mut tag_counts = HashMap::new();
    for (tag, _) in tags.iter() {
      tag_counts.insert(*tag, [0, 0]);
    }

    Tester {
      fails: vec![],
      count_correct: 0,
      count_failed: 0,
			tags,
      tag_counts
    }
  }

  pub fn test_many(&mut self, criteria: WhiteSpace, snippets: &[&str]) {
    let compiled_snippets: Vec<(String, bool)> = tsc_compile_snippets(&snippets);

    for (_idx, (source, (tsc_output, tsc_is_ok))) in snippets.iter().zip(compiled_snippets).enumerate() {
      let state = test_code(source, &tsc_output, criteria.clone());
      let result_idx = if state.is_err() {
        self.fails.push(state.err().unwrap());
        self.count_failed += 1;
        1
      } else {
        self.count_correct += 1;
        0
      };
      for (tag, lookups) in self.tags.iter() {
        let mut found = false;
        for l in lookups {
          let padded_source = " ".to_owned() + source;
          let a = l.captures_iter(&padded_source);
          if a.count() > 0 {
            found = true;
            break;
          }
        }
        if !found { continue; }
        
        let runs = unsafe { self.tag_counts.get_mut(tag).unwrap_unchecked() };
        runs[result_idx] += 1;
      }
    }
  }

	pub fn finish(&mut self) {
		if self.count_failed == 0 { return; }

		for fail in &self.fails {
			println!("{}", fail);
		}

		let mut counts: Vec<(&str, &[usize; 2])> = vec![];
		for tag in self.tag_counts.iter() {
			counts.push((tag.0, tag.1));
		}
		counts.sort_by_key(|k| {
			(
				(k.1[0] as f64 / (k.1[0] + k.1[1]) as f64) * 1000000.0
			) as i64 - (k.1[0] + k.1[1]) as i64
		});

		let mut missing_tests = 0;
		for (tag, [ win, loss ]) in counts.iter() {
			if win + loss < REQUIRED_TESTS {
				missing_tests += 1;
				continue;
			}
			Self::print_win_loss(tag, *win, *loss);
		}

		println!();
		Self::print_win_loss("Total", self.count_correct, self.count_failed);

		if missing_tests > 0 {
			println!("\nNote: {} tags had less than {} tests:", missing_tests, REQUIRED_TESTS);
			for (tag, [ win, loss ]) in counts.iter() {
				if win + loss < REQUIRED_TESTS {
					println!("  {} has {} test(s)", tag, win + loss);
				}
			}
		}

		panic!();
	}

	fn print_win_loss(msg: &str, win: usize, loss: usize) {
		println!(
			"{: <16}: {: >4} correct, {: >4} failed ({:.2}% passed)",
			msg, win, loss,
			(win as f64 / (win + loss) as f64) * 100.0
		);
	}
}

pub fn tsc_compile_snippets(ts_snippets: &[&str]) -> Vec<(String, bool)> {
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
pub fn test_code(source: &str, compiled: &str, whitespace: WhiteSpace) -> Result<(), String> {
  let mut code_string = source.to_string();
  code_string += "\n ";
  let code = code_string.as_str();

  // Get the `rstsc` output
  let out = std::panic::catch_unwind(|| {
    rstsc::compile(code, false)
  });
  let mut actual: String = match out {
    Ok(Ok(code)) => code.clone(),
    Ok(Err(err)) => format!("{:?}", err),
    Err(err) => {
			println!("RSTSC error: {:?}", code);
			format!("{:?}", err)
		}
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
