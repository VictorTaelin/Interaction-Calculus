mod extra;
mod net;
mod term;

use extra::*;
use term::*;

use clap::{App, Arg};
use std::fs::File;
use std::io;
use std::io::prelude::*;

fn main() -> io::Result<()> {
    let matches = App::new("Symmetric Interaction Calculus")
        .version("0.1.0")
        .author("Victor Maia <srvictormaia@gmail.com>")
        .about("Evaluates SIC programs")
        .arg(
            Arg::new("INPUT")
                .short('i')
                .long("input")
                .value_name("INPUT")
                .help("Input term")
                .takes_value(true),
        )
        .arg(
            Arg::new("AINPUT")
                .short('a')
                .long("ainput")
                .value_name("AINPUT")
                .help("Input term, encoded as ascii")
                .takes_value(true),
        )
        .arg(
            Arg::new("BINPUT")
                .short('b')
                .long("binput")
                .value_name("BINPUT")
                .help("Input term, encoded as a binary string")
                .takes_value(true),
        )
        .arg(
            Arg::new("BOUTPUT")
                .short('B')
                .long("boutput")
                .value_name("BOUTPUT")
                .help("Decodes output as a binary string")
                .takes_value(false),
        )
        .arg(
            Arg::new("AOUTPUT")
                .short('A')
                .long("aoutput")
                .value_name("AOUTPUT")
                .help("Decodes output as ascii")
                .takes_value(false),
        )
        .arg(
            Arg::new("STATS")
                .short('s')
                .long("stats")
                .value_name("STATS")
                .help("Show stats")
                .takes_value(false),
        )
        .arg(
            Arg::new("FILE")
                .help("Sets the input file to use")
                .required(true)
                .index(1),
        )
        .get_matches();

    let file_name = matches.value_of("FILE").unwrap();
    let mut file = File::open(file_name)?;
    let mut code = Vec::new();
    file.read_to_end(&mut code)?;

    let input: Option<Vec<u8>> = match matches.value_of("AINPUT") {
        Some(ascii) => Some(to_string(&bitstring_to_term(
            &ascii_to_bits(ascii.as_bytes()),
            0,
        ))),
        None => match matches.value_of("BINPUT") {
            Some(bits) => Some(to_string(&bitstring_to_term(bits.as_bytes(), 0))),
            None => matches
                .value_of("INPUT")
                .map(|term| term.as_bytes().to_vec()),
        },
    };

    if let Some(mut input) = input {
        code.extend_from_slice(b"\n:main ");
        code.append(&mut input);
    }

    let term = from_string(&code);
    let mut net = to_net(&term);
    let stats = net::reduce(&mut net);
    let norm = from_net(&net);

    let output = if matches.is_present("BOUTPUT") {
        term_to_bitstring(&norm)
    } else if matches.is_present("AOUTPUT") {
        bits_to_ascii(&term_to_bitstring(&norm))
    } else {
        to_string(&norm)
    };

    println!("{}", String::from_utf8_lossy(&output));

    if matches.is_present("STATS") {
        println!("{:?}", stats);
    }

    Ok(())
}
