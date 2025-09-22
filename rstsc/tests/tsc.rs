use rayon::prelude::*;
mod common;
use std::collections::HashMap;
use regex::Regex;

use common::{test_code, tsc_compile_snippets};

const SOURCE: &str = include_str!("./tests.ts");
const REQUIRED_TESTS: usize = 4;

#[test]
fn tsc_tests() {
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

  let mut fails = vec![];
  let mut count_correct = 0;
  let mut count_failed = 0;
  let mut tag_counts = HashMap::new();

  // Add tags to hashmap
  for (tag, _) in tags.iter() {
    tag_counts.insert(tag, [ 0, 0 ]);
  }

  let source_snippets: Vec<&str> = SOURCE.split("\n\n").collect();
  let compiled_snippets: Vec<(String, bool)> = tsc_compile_snippets(&source_snippets);
  // source_snippets.par_iter().map(|s| {
  //   match tsc_compile(s) {
  //     Ok(result) => ( result, true ),
  //     Err(err) => ( err, false )
  //   }
  // }).collect();

  for (_idx, (source, (tsc_output, tsc_is_ok))) in source_snippets.iter().zip(compiled_snippets).enumerate() {
    let state = test_code(source, &tsc_output, &common::WhiteSpace::IgnoreAll);
    let result_idx = if state.is_err() {
      fails.push(state.err().unwrap());
      count_failed += 1;
      1
    } else {
      count_correct += 1;
      0
    };
    for (tag, lookups) in tags.iter() {
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
      
      let runs = unsafe { tag_counts.get_mut(tag).unwrap_unchecked() };
      runs[result_idx] += 1;
    }
  }

  if count_failed > 0 {
    // Some fails
    for fail in fails {
      println!("{}", fail);
    }

    let mut counts: Vec<(&str, &[usize; 2])> = vec![];
    for tag in tag_counts.iter() {
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
      print_win_loss(tag, *win, *loss);
    }

    println!();
    print_win_loss("Total", count_correct, count_failed);

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
}

fn print_win_loss(msg: &str, win: usize, loss: usize) {
  println!(
    "{: <16}: {: >4} correct, {: >4} failed ({:.2}% passed)",
    msg, win, loss,
    (win as f64 / (win + loss) as f64) * 100.0
  );
}
