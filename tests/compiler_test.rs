use assert_cmd::cargo;
use std::fs;
use std::path::{Path, PathBuf};

fn find_hk_files(dir: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    for entry in fs::read_dir(dir).expect("Failed to read test directory") {
        let entry = entry.expect("Failed to read entry");
        let path = entry.path();
        if path.is_dir() {
            files.extend(find_hk_files(&path));
        } else if path.extension().and_then(|s| s.to_str()) == Some("hk") {
            files.push(path);
        }
    }
    files
}

fn extract_expected_output(contents: &str) -> Option<String> {
    let start_tag = "<expect>";
    let end_tag = "</expect>";
    let start = contents.find(start_tag)? + start_tag.len();
    let end = contents.find(end_tag)?;
    Some(contents[start..end].trim().to_string())
}

fn is_expected_to_fail(contents: &str) -> bool {
    contents.contains("<fail>")
}

fn remove_build_folders(dir: &Path) {
    if !dir.is_dir() {
        return;
    }
    for entry in fs::read_dir(dir).expect("Failed to read directory") {
        let entry = entry.expect("Failed to read entry");
        let path = entry.path();
        if path.is_dir() {
            if path.file_name().unwrap() == "build" {
                fs::remove_dir_all(&path).unwrap_or_else(|_| {
                    panic!("Failed to remove build directory: {}", path.display())
                });
                println!("Removed build directory: {}", path.display());
            } else {
                remove_build_folders(&path);
            }
        }
    }
}

#[test]
fn hk_golden_tests() {
    let test_dir = Path::new("tests");

    let files = find_hk_files(test_dir);

    for path in files {
        let contents = fs::read_to_string(&path)
            .unwrap_or_else(|_| panic!("Failed to read {}", path.display()));
        let expected_output = extract_expected_output(&contents).unwrap_or_default();
        let should_fail = is_expected_to_fail(&contents);

        let mut cmd = cargo::cargo_bin_cmd!("compiler");
        let output = cmd.arg(&path).output().expect("Failed to run compiler");
        let stdout = String::from_utf8_lossy(&output.stdout).trim().to_string();

        let passed = if should_fail {
            !output.status.success()
        } else {
            output.status.success() && stdout == expected_output.clone()
        };

        print!(
            "\nTesting {}. Expected Output: {:?}, Got: {:?}, Passed: {}",
            path.file_name().unwrap().to_string_lossy(),
            expected_output,
            stdout,
            passed
        );

        assert!(passed, "Test failed for {}", path.display());
    }

    println!("\nAll tests completed.");

    remove_build_folders(test_dir);
}
