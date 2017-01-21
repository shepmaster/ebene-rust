extern crate strata_rs;
extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;

use std::fs::File;
use std::env;
use std::io::prelude::*;

#[derive(Debug, Default, Serialize, Deserialize)]
struct Done {
    functions: Vec<strata_rs::Extent>,
    idents: Vec<strata_rs::Extent>,
}

fn main() {
    let fname = env::args().nth(1).expect("Need filename argument");
    let mut f = File::open(fname).expect("Can't open");
    let mut s = String::new();
    f.read_to_string(&mut s).expect("Can't read");
    let file = strata_rs::parse_rust_file(&s).expect("Failed");
//    println!("{:#?}", file);
    let mut d = Done::default();

    for i in &file {
        use strata_rs::TopLevel::*;

        if let Function(ref f) = *i {
//            let name = f.header.name;
//            println!("{:?}, {:?}, {}", i.extent(), name, &s[name.0..name.1]);
            d.functions.push(i.extent());
            d.idents.push(f.header.name);
        }
    }

    let mut out = std::io::stdout();
    serde_json::ser::to_writer(&mut out, &d).expect("Nope");
}

// Goal:
// What methods take a generic argument that implements a trait `Foo`
