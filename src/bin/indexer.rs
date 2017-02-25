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

#[derive(Debug, Default)]
struct Indexing {
    source: String,

    term_ident: BTreeMap<String, BTreeSet<strata_rs::Extent>>,

    layer_enum: BTreeSet<strata_rs::Extent>,
    layer_function: BTreeSet<strata_rs::Extent>,
    layer_function_header: BTreeSet<strata_rs::Extent>,
    layer_struct: BTreeSet<strata_rs::Extent>,
    layer_generic_declarations: BTreeSet<strata_rs::Extent>,
    layer_where: BTreeSet<strata_rs::Extent>,

    layers_expression: Vec<BTreeSet<strata_rs::Extent>>,
    layers_statement: Vec<BTreeSet<strata_rs::Extent>>,

    stmt_depth: usize,
    expr_depth: usize,
}

impl std::ops::Index<strata_rs::Extent> for Indexing {
    type Output = str;

    fn index(&self, extent: strata_rs::Extent) -> &str {
        &self.source[extent.0..extent.1]
    }
}

impl Visitor for Indexing {
    fn visit_ident(&mut self, ident: &strata_rs::Ident) {
        let s = self[ident.extent].to_owned();
        self.term_ident.entry(s).or_insert_with(BTreeSet::new).insert(ident.extent);
    }

    fn visit_function(&mut self, function: &strata_rs::Function) {
        self.layer_function.insert(function.extent);
    }

    fn visit_function_header(&mut self, header: &strata_rs::FunctionHeader) {
        self.layer_function_header.insert(header.extent);
    }

    fn visit_enum(&mut self, e: &strata_rs::Enum) {
        self.layer_enum.insert(e.extent);
    }

    fn visit_struct(&mut self, s: &strata_rs::Struct) {
        self.layer_struct.insert(s.extent);
    }

    fn visit_generic_declarations(&mut self, gd: &strata_rs::GenericDeclarations) {
        self.layer_generic_declarations.insert(gd.extent);
    }

    fn visit_where(&mut self, w: &strata_rs::Where) {
        self.layer_where.insert(w.extent);
    }

    fn visit_statement(&mut self, s: &strata_rs::Statement) {
        while self.layers_statement.len() <= self.stmt_depth {
            self.layers_statement.push(BTreeSet::new());
        }
        self.layers_statement[self.stmt_depth].insert(s.extent());

        self.stmt_depth +=1
    }

    fn exit_statement(&mut self, _: &strata_rs::Statement) {
        self.stmt_depth -= 1;
    }

    fn visit_expression(&mut self, e: &strata_rs::Expression) {
        while self.layers_expression.len() <= self.expr_depth {
            self.layers_expression.push(BTreeSet::new());
        }
        self.layers_expression[self.expr_depth].insert(e.extent());

        self.expr_depth += 1;
    }

    fn exit_expression(&mut self, _: &strata_rs::Expression) {
        self.expr_depth -= 1;
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
struct IndexedFile {
    source: String,
    layers: BTreeMap<String, BTreeSet<strata_rs::Extent>>,
    terms: BTreeMap<String, BTreeMap<String, BTreeSet<strata_rs::Extent>>>,
}

// TODO: This was extracted from `strata`; should be made public
fn find_invalid_gc_list_pair(extents: &[strata_rs::Extent]) -> Option<(strata_rs::Extent, strata_rs::Extent)> {
    extents
        .windows(2)
        .map(|window| (window[0], window[1]))
        .find(|&(a, b)| b.0 <= a.0 || b.1 <= a.1)
}

impl IndexedFile {
    fn validate(&self) {
        for (layer_name, layer_extents) in &self.layers {
            let layer_extents: Vec<_> = layer_extents.iter().cloned().collect();
            if let Some(bad) = find_invalid_gc_list_pair(&layer_extents) {
                println!("WARNING: Layer {} has invalid extents: {:?}", layer_name, bad);
            }
        }
    }
}

impl From<Indexing> for IndexedFile {
    fn from(other: Indexing) -> IndexedFile {
        let Indexing {
            source,

            term_ident,

            layer_enum,
            layer_function,
            layer_function_header,
            layer_struct,
            layer_generic_declarations,
            layer_where,

            layers_expression,
            layers_statement,

            ..
        } = other;

        let mut layers = BTreeMap::new();
        layers.insert("enum".into(), layer_enum);
        layers.insert("function".into(), layer_function);
        layers.insert("function-header".into(), layer_function_header);
        layers.insert("struct".into(), layer_struct);
        layers.insert("generic-declarations".into(), layer_generic_declarations);
        layers.insert("where".into(), layer_where);

        for (i, layer) in layers_expression.into_iter().enumerate() {
            layers.insert(format!("expression-{}", i), layer);
        }
        for (i, layer) in layers_statement.into_iter().enumerate() {
            layers.insert(format!("statement-{}", i), layer);
        }

        let mut terms = BTreeMap::new();
        terms.insert("ident".into(), term_ident);

        IndexedFile { source, terms, layers }
    }
}

fn main() {
    let fname = env::args().nth(1).expect("Need filename argument");
    let mut f = File::open(fname).expect("Can't open");
    let mut s = String::new();
    f.read_to_string(&mut s).expect("Can't read");

    let file = match strata_rs::parse_rust_file(&s) {
        Ok(file) => file,
        Err(detail) => {
            panic!("{}", detail.with_text(&s));
        }
    };

    let mut d = Indexing::default();
    d.source = s;

    file.visit(&mut d);

    let d: IndexedFile = d.into();
    d.validate();

    let mut out = std::io::stdout();
    serde_json::ser::to_writer(&mut out, &d).expect("Nope");
}

// Goal:
// What methods take a generic argument that implements a trait `Foo`
