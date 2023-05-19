#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_mut)]
#![allow(unused_parens)]
#![allow(unused_variables)]
#![allow(unreachable_code)]

extern crate clap;
use clap::{Arg, App};

mod term;
mod inet;
mod test;

use term::*;

use std::io;
use std::io::prelude::*;
use std::fs::File;

fn main() {
  //return test::test();

  let matches = App::new("My App")
    .version("1.0")
    .author("Victor Taelin <victor.taelin@gmail.com>")
    .about("Interaction Calculus CLI")
    .arg(Arg::with_name("file")
      .help("The input file to process")
      .required(true)
      .index(1))
    .get_matches();

  let file_name = matches.value_of("file").unwrap();
  let mut file = File::open(file_name).expect("Unable to open the file");
  let mut code = String::new();
  file.read_to_string(&mut code).expect("Unable to read the file");

  let term = term::from_string(code.as_bytes());
  let (norm, rules) = term::normal_with_stats(&term);

  println!("{}\n", norm);
  println!("{:?} rewrites", rules);
}
