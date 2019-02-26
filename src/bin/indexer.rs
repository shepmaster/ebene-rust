extern crate fuzzy_pickles;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;

use std::fs::File;
use std::env;
use std::io::prelude::*;
use std::collections::{BTreeMap, BTreeSet};

use fuzzy_pickles::{ast, visit::{Visit, Visitor, Control}, HasExtent};

#[derive(Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Serialize, Deserialize)]
struct Extent(usize, usize);

impl From<fuzzy_pickles::Extent> for Extent {
    fn from(other: fuzzy_pickles::Extent) -> Self {
        Extent(other.0, other.1)
    }
}

#[derive(Debug, Default)]
struct Indexing {
    source: String,

    term_ident: BTreeMap<String, BTreeSet<Extent>>,

    layer_enum: BTreeSet<Extent>,
    layer_function: BTreeSet<Extent>,
    layer_function_header: BTreeSet<Extent>,
    layer_struct: BTreeSet<Extent>,
    layer_generic_declarations: BTreeSet<Extent>,
    layer_where: BTreeSet<Extent>,

    layers_expression: Vec<BTreeSet<Extent>>,
    layers_statement: Vec<BTreeSet<Extent>>,

    stmt_depth: usize,
    expr_depth: usize,
}

impl std::ops::Index<fuzzy_pickles::Extent> for Indexing {
    type Output = str;

    fn index(&self, extent: fuzzy_pickles::Extent) -> &str {
        &self.source[extent.0..extent.1]
    }
}

impl Visitor<'_> for Indexing {
    fn visit_ident(&mut self, ident: &ast::Ident) -> Control {
        let s = self[ident.extent].to_owned();
        self.term_ident.entry(s).or_insert_with(BTreeSet::new).insert(ident.extent.into());
        Control::Continue
    }

    fn visit_function(&mut self, function: &ast::Function) -> Control {
        self.layer_function.insert(function.extent.into());
        Control::Continue
    }

    fn visit_function_header(&mut self, header: &ast::FunctionHeader) -> Control {
        self.layer_function_header.insert(header.extent.into());
        Control::Continue
    }

    fn visit_enum(&mut self, e: &ast::Enum) -> Control {
        self.layer_enum.insert(e.extent.into());
        Control::Continue
    }

    fn visit_struct(&mut self, s: &ast::Struct) -> Control {
        self.layer_struct.insert(s.extent.into());
        Control::Continue
    }

    fn visit_generic_declarations(&mut self, gd: &ast::GenericDeclarations) -> Control {
        self.layer_generic_declarations.insert(gd.extent.into());
        Control::Continue
    }

    fn visit_where(&mut self, w: &ast::Where) -> Control {
        self.layer_where.insert(w.extent().into());
        Control::Continue
    }

    fn visit_statement(&mut self, s: &ast::Statement) -> Control {
        while self.layers_statement.len() <= self.stmt_depth {
            self.layers_statement.push(BTreeSet::new());
        }
        self.layers_statement[self.stmt_depth].insert(s.extent().into());

        self.stmt_depth +=1;
        Control::Continue
    }

    fn exit_statement(&mut self, _: &ast::Statement) {
        self.stmt_depth -= 1;
    }

    fn visit_expression(&mut self, e: &ast::Expression) -> Control {
        while self.layers_expression.len() <= self.expr_depth {
            self.layers_expression.push(BTreeSet::new());
        }
        self.layers_expression[self.expr_depth].insert(e.extent().into());

        self.expr_depth += 1;
        Control::Continue
    }

    fn exit_expression(&mut self, _: &ast::Expression) {
        self.expr_depth -= 1;
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
struct IndexedFile {
    source: String,
    layers: BTreeMap<String, BTreeSet<Extent>>,
    terms: BTreeMap<String, BTreeMap<String, BTreeSet<Extent>>>,
}

// TODO: This was extracted from `strata`; should be made public
fn find_invalid_gc_list_pair(extents: &[Extent]) -> Option<(Extent, Extent)> {
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
