extern crate strata_rs;

use std::fs::File;
use std::env;
use std::io::prelude::*;

fn main() {
    let fname = env::args().nth(1).expect("Need filename argument");
    let mut f = File::open(fname).expect("Can't open");
    let mut s = String::new();
    f.read_to_string(&mut s).expect("Can't read");
    let file = strata_rs::parse_rust_file(&s).expect("Failed");
    println!("{:#?}", file);
}

// Goal:
// What methods take a generic argument that implements a trait `Foo`
