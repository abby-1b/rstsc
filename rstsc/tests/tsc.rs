mod common;
use std::collections::HashMap;
use regex::Regex;

use common::test_code;

const SOURCE: &str = include_str!("./tests.ts");

#[test]
fn tsc_tests() {
    let pre_tags: Vec<(&str, &[&str])> = vec![
        ("Function", &[ r"function " ]),
        ("Variable", &[ r"const ", r"let ", r"var " ]),
        ("Type Decl", &[ r"type " ]),
        ("Interface", &[ r"interface " ]),
        ("Class", &[ r"[^a-zA-Z]class( |<)" ]),
        ("Enum", &[ r"enum " ]),
        ("Generic", &[ r"<[a-zA-Z]*?>" ]),
        ("Array", &[ r"\[", r"\]" ]),
        ("T-Condition", &[ r" extends .*? :" ]),
        ("T-Key", &[ r"\[( |)key( |):[ a-zA-Z]*?\]:" ]),
        ("Arrow Fn", &[ r"=>" ]),
        ("Spread", &[ r"..." ]),
        ("Getter", &[ r"get " ]),
        ("Setter", &[ r"set " ]),
    ];

    // Compile tags into regex
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

    for test_source in SOURCE.split("\n\n") {
        let state = test_code(test_source, &common::WhiteSpace::IgnoreAll);
        let idx = if state.is_err() {
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
                let a = l.captures_iter(test_source);
                if a.count() > 0 {
                    found = true;
                    break;
                }
            }
            if !found { continue; }
            
            let runs = unsafe { tag_counts.get_mut(tag).unwrap_unchecked() };
            runs[idx] += 1;
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
            ((k.1[0] as f64 / (k.1[0] + k.1[1]) as f64) * 100000.0) as usize
        });

        for (tag, [ win, loss ]) in counts {
            print_win_loss(tag, *win, *loss);
        }
        println!();
        print_win_loss("Total", count_correct, count_failed);
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
