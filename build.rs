use std::process::Command;
fn main() {
  // note: add error checking yourself.
  let output = Command::new("git").args(&["log", "-1"]).output().expect("Failed to execute: git rev-parse HEAD");
  let git_rev = String::from_utf8(output.stdout).unwrap();
  println!("cargo:rustc-env=GIT_REV={}", git_rev);
  println!("cargo:rustc-env=BUILD_AT={}", chrono::Local::now().to_rfc3339_opts(chrono::SecondsFormat::Secs, false));
}
