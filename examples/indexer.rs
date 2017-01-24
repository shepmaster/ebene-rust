extern crate strata_rs;
extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;

use std::fs::File;
use std::env;
use std::io::prelude::*;
use std::collections::{BTreeMap, BTreeSet};

use strata_rs::{Visit, Visitor};

#[derive(Debug, Default, Serialize, Deserialize)]
struct IndexedFile {
    source: String,
    functions: Vec<strata_rs::Extent>,
    idents: Vec<strata_rs::Extent>,
    enums: Vec<strata_rs::Extent>,
    structs: Vec<strata_rs::Extent>,
    terms: BTreeMap<String, BTreeMap<String, BTreeSet<strata_rs::Extent>>>,
}

impl IndexedFile {
    fn finalize(&mut self) {
        let mut ident_terms = BTreeMap::new();

        for &ex in &self.idents {
            let s = self[ex].to_owned();
            ident_terms.entry(s).or_insert_with(BTreeSet::new).insert(ex);
        }

        self.terms.insert("ident".into(), ident_terms);
    }
}

impl std::ops::Index<strata_rs::Extent> for IndexedFile {
    type Output = str;

    fn index(&self, extent: strata_rs::Extent) -> &str {
        &self.source[extent.0..extent.1]
    }
}

impl Visitor for IndexedFile {
    fn visit_ident(&mut self, ident: &strata_rs::Ident) {
        self.idents.push(ident.extent);
    }

    fn visit_function(&mut self, function: &strata_rs::Function) {
        self.functions.push(function.extent);
    }

    fn visit_enum(&mut self, e: &strata_rs::Enum) {
        self.enums.push(e.extent);
    }

    fn visit_struct(&mut self, s: &strata_rs::Struct) {
        self.structs.push(s.extent);
    }
}

fn main() {
    let fname = env::args().nth(1).expect("Need filename argument");
    let mut f = File::open(fname).expect("Can't open");
    let mut s = String::new();
    f.read_to_string(&mut s).expect("Can't read");
    let file = strata_rs::parse_rust_file(&s).expect("Failed");
    let mut d = IndexedFile::default();
    d.source = s;

    for i in &file {
        i.visit(&mut d);
    }

    d.finalize();

    let mut out = std::io::stdout();
    serde_json::ser::to_writer(&mut out, &d).expect("Nope");
}

// Goal:
// What methods take a generic argument that implements a trait `Foo`
