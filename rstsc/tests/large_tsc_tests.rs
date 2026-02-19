mod common;
use crate::common::{Tester, WhiteSpace};
use std::fs;
use std::path::{Path, PathBuf};
use std::thread;

const IS_PARALLEL: bool = false;
const THREAD_COUNT: usize = 4;

const BATCH_SIZE: usize = 32;

#[test]
fn large_tsc_tests() {
  return;
  let directory = "target/test/tsc";

  // --- Git Setup (Unchanged) ---
  std::fs::create_dir_all("target/test").unwrap();
  if !std::path::Path::new(directory).exists() {
    std::process::Command::new("git")
      .args(&["clone", "--filter=blob:none", "--no-checkout", "--depth=1", "https://github.com/microsoft/TypeScript.git", directory])
      .status()
      .unwrap();

    std::process::Command::new("git")
      .args(&["sparse-checkout", "set", "--no-cone", "tests/cases/conformance/*"])
      .current_dir(directory)
      .status()
      .unwrap();

    std::process::Command::new("git")
      .args(&["checkout", "main"])
      .current_dir(directory)
      .status()
      .unwrap();
  }

  // --- File Discovery ---
  let conformance_path = Path::new(directory).join("tests/cases/conformance");
  let mut all_files = Vec::new();
  collect_ts_files(&conformance_path, &mut all_files);

  if all_files.is_empty() {
    return;
  }

  // --- Execution Logic ---
  if IS_PARALLEL {
    run_parallel(all_files);
  } else {
    run_sequential(all_files);
  }
}

/// Logic for processing a list of paths in batches with a single tester
fn process_in_batches(tester: &mut Tester, files: Vec<PathBuf>) {
  for batch in files.chunks(BATCH_SIZE) {
    let mut source_contents = Vec::with_capacity(batch.len());

    for path in batch {
      if let Ok(content) = fs::read_to_string(path) {
        source_contents.push(content);
      }
    }

    let source_snippets: Vec<&str> = source_contents.iter().map(|s| s.as_str()).collect();
    tester.test_many(WhiteSpace::IgnoreAll, &source_snippets);
    // source_contents dropped here, keeping memory low
  }
  tester.finish();
}

fn run_sequential(all_files: Vec<PathBuf>) {
  let mut tester = Tester::new();
  process_in_batches(&mut tester, all_files);
}

fn run_parallel(all_files: Vec<PathBuf>) {
  let total_files = all_files.len();
  let chunk_size = (total_files + THREAD_COUNT - 1) / THREAD_COUNT;
  
  let mut handles = Vec::new();
  for file_chunk in all_files.chunks(chunk_size) {
    let chunk_vec = file_chunk.to_vec();
    let handle = thread::spawn(move || {
      let mut tester = Tester::new();
      process_in_batches(&mut tester, chunk_vec);
    });
    handles.push(handle);
  }

  for handle in handles {
    handle.join().unwrap();
  }
}

fn collect_ts_files(dir: &Path, files: &mut Vec<PathBuf>) {
  if let Ok(entries) = fs::read_dir(dir) {
    for entry in entries.flatten() {
      let path = entry.path();
      if path.is_dir() {
        collect_ts_files(&path, files);
      } else if path.extension().map_or(false, |ext| ext == "ts") {
        files.push(path);
      }
    }
  }
}