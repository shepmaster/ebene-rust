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

// This is a bit inefficient:
// 1. We add identifiers to the layers, but probably don't want to search that.
// 1. We have to do a lot of extra allocation for all the `entry` calls
// We could split this into two types, performing the finalize step at conversion
#[derive(Debug, Default, Serialize, Deserialize)]
struct IndexedFile {
    source: String,
    layers: BTreeMap<String, BTreeSet<strata_rs::Extent>>,
    terms: BTreeMap<String, BTreeMap<String, BTreeSet<strata_rs::Extent>>>,
    stmt_depth: usize,
    expr_depth: usize,
}

// TODO: This was extracted from `strata`; should be made public
fn find_invalid_gc_list_pair(extents: &[strata_rs::Extent]) -> Option<(strata_rs::Extent, strata_rs::Extent)> {
    extents
        .windows(2)
        .map(|window| (window[0], window[1]))
        .find(|&(a, b)| b.0 <= a.0 || b.1 <= a.1)
}

impl IndexedFile {
    fn add_extent<S>(&mut self, layer: S, extent: strata_rs::Extent)
        where S: Into<String>,
    {
        self.layers.entry(layer.into()).or_insert_with(BTreeSet::new).insert(extent);
    }

    fn finalize(&mut self) {
        let mut ident_terms = BTreeMap::new();

        for &ex in self.layers.get("ident").expect("No identifiers present") {
            let s = self[ex].to_owned();
            ident_terms.entry(s).or_insert_with(BTreeSet::new).insert(ex);
        }

        for (layer_name, layer_extents) in &self.layers {
            let layer_extents: Vec<_> = layer_extents.iter().cloned().collect();
            if let Some(bad) = find_invalid_gc_list_pair(&layer_extents) {
                println!("WARNING: Layer {} has invalid extents: {:?}", layer_name, bad);
            }
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
        self.add_extent("ident", ident.extent);
    }

    fn visit_function(&mut self, function: &strata_rs::Function) {
        self.add_extent("function", function.extent);
    }

    fn visit_enum(&mut self, e: &strata_rs::Enum) {
        self.add_extent("enum", e.extent);
    }

    fn visit_struct(&mut self, s: &strata_rs::Struct) {
        self.add_extent("struct", s.extent);
    }

    fn visit_statement(&mut self, s: &strata_rs::Statement) {
        let name = format!("statement-{}", self.stmt_depth);
        self.add_extent(name, s.extent());
        self.stmt_depth +=1
    }

    fn exit_statement(&mut self, _: &strata_rs::Statement) {
        self.stmt_depth -= 1;
    }

    fn visit_expression(&mut self, e: &strata_rs::Expression) {
        let name = format!("expression-{}", self.expr_depth);
        self.add_extent(name, e.extent());
        self.expr_depth += 1;
    }

    fn exit_expression(&mut self, _: &strata_rs::Expression) {
        self.expr_depth -= 1;
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
