use std::process::Command;
use base64::prelude::*;

fn main() {
  // note: add error checking yourself.
  let git_rev = Command::new("git").args(&["log", "-1"]).output().expect("Failed to execute: git log -1");
  println!("cargo:rustc-env=GIT_REV={}", BASE64_STANDARD.encode(git_rev.stdout));
  println!("cargo:rustc-env=BUILD_AT={}", chrono::Local::now().to_rfc3339_opts(chrono::SecondsFormat::Secs, false));
}
