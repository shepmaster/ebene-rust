extern crate fuzzy_pickles;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;

use std::fs::File;
use std::env;
use std::io::prelude::*;
use std::collections::{BTreeMap, BTreeSet};

use fuzzy_pickles::{Visit, Visitor, HasExtent};

#[derive(Debug, Default)]
struct Indexing {
    source: String,

    term_ident: BTreeMap<String, BTreeSet<fuzzy_pickles::Extent>>,

    layer_enum: BTreeSet<fuzzy_pickles::Extent>,
    layer_function: BTreeSet<fuzzy_pickles::Extent>,
    layer_function_header: BTreeSet<fuzzy_pickles::Extent>,
    layer_struct: BTreeSet<fuzzy_pickles::Extent>,
    layer_generic_declarations: BTreeSet<fuzzy_pickles::Extent>,
    layer_where: BTreeSet<fuzzy_pickles::Extent>,

    layers_expression: Vec<BTreeSet<fuzzy_pickles::Extent>>,
    layers_statement: Vec<BTreeSet<fuzzy_pickles::Extent>>,

    stmt_depth: usize,
    expr_depth: usize,
}

impl std::ops::Index<fuzzy_pickles::Extent> for Indexing {
    type Output = str;

    fn index(&self, extent: fuzzy_pickles::Extent) -> &str {
        &self.source[extent.0..extent.1]
    }
}

impl Visitor for Indexing {
    fn visit_ident(&mut self, ident: &fuzzy_pickles::Ident) {
        let s = self[ident.extent].to_owned();
        self.term_ident.entry(s).or_insert_with(BTreeSet::new).insert(ident.extent);
    }

    fn visit_function(&mut self, function: &fuzzy_pickles::Function) {
        self.layer_function.insert(function.extent);
    }

    fn visit_function_header(&mut self, header: &fuzzy_pickles::FunctionHeader) {
        self.layer_function_header.insert(header.extent);
    }

    fn visit_enum(&mut self, e: &fuzzy_pickles::Enum) {
        self.layer_enum.insert(e.extent);
    }

    fn visit_struct(&mut self, s: &fuzzy_pickles::Struct) {
        self.layer_struct.insert(s.extent);
    }

    fn visit_generic_declarations(&mut self, gd: &fuzzy_pickles::GenericDeclarations) {
        self.layer_generic_declarations.insert(gd.extent);
    }

    fn visit_where(&mut self, w: &fuzzy_pickles::Where) {
        self.layer_where.insert(w.extent());
    }

    fn visit_statement(&mut self, s: &fuzzy_pickles::Statement) {
        while self.layers_statement.len() <= self.stmt_depth {
            self.layers_statement.push(BTreeSet::new());
        }
        self.layers_statement[self.stmt_depth].insert(s.extent());

        self.stmt_depth +=1
    }

    fn exit_statement(&mut self, _: &fuzzy_pickles::Statement) {
        self.stmt_depth -= 1;
    }

    fn visit_expression(&mut self, e: &fuzzy_pickles::Expression) {
        while self.layers_expression.len() <= self.expr_depth {
            self.layers_expression.push(BTreeSet::new());
        }
        self.layers_expression[self.expr_depth].insert(e.extent());

        self.expr_depth += 1;
    }

    fn exit_expression(&mut self, _: &fuzzy_pickles::Expression) {
        self.expr_depth -= 1;
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
struct IndexedFile {
    source: String,
    layers: BTreeMap<String, BTreeSet<fuzzy_pickles::Extent>>,
    terms: BTreeMap<String, BTreeMap<String, BTreeSet<fuzzy_pickles::Extent>>>,
}

// TODO: This was extracted from `strata`; should be made public
fn find_invalid_gc_list_pair(extents: &[fuzzy_pickles::Extent]) -> Option<(fuzzy_pickles::Extent, fuzzy_pickles::Extent)> {
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

    let file = match fuzzy_pickles::parse_rust_file(&s) {
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
