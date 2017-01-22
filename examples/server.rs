#![feature(field_init_shorthand)]
#![feature(plugin, custom_derive)]
#![plugin(rocket_codegen)]

extern crate strata;
extern crate strata_rs;

extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;

#[macro_use]
extern crate lazy_static;

extern crate rocket;
extern crate rocket_contrib;

use std::fs::File;
use std::collections::HashMap;

use strata::{Algebra, ValidExtent};

use rocket_contrib::JSON;

lazy_static! {
    static ref INDEX: Indexed = {
        let f = File::open("self.json").expect("no open file");
        serde_json::de::from_reader(f).expect("no parse")
    };
    static ref IDENT_INDEX: HashMap<String, Vec<ValidExtent>> = {
        let mut index = HashMap::new();
        for ex in &INDEX.idents {
            let s = INDEX.source[(ex.0 as usize)..(ex.1 as usize)].to_owned();
            index.entry(s).or_insert_with(Vec::new).push(ex.clone());
        }
        index
    };
}

#[derive(Debug, Default, Serialize, Deserialize)]
struct Indexed {
    source: String,
    functions: Vec<strata::ValidExtent>,
    idents: Vec<strata::ValidExtent>, // mismatched type usize / u64!
}

impl std::ops::Index<strata::ValidExtent> for Indexed {
    type Output = str;

    fn index(&self, extent: ValidExtent) -> &str {
        &self.source[(extent.0 as usize)..(extent.1 as usize)]
    }
}

#[derive(Debug, Serialize)]
struct Homepage {
    source: String,
}

#[derive(Debug, Serialize)]
struct Ex {
    extents: Vec<ValidExtent>,
}

#[get("/idents/<ident>")]
fn idents(ident: &str) -> JSON<Ex> {
    let idents = IDENT_INDEX.get(ident).cloned().unwrap_or_else(Vec::new);
    JSON(Ex { extents: idents })
}

#[get("/")]
fn index() -> JSON<Homepage> {
    JSON(Homepage { source: INDEX.source.clone() })
}

#[derive(Debug, Deserialize, FromForm)]
struct Query {
    q: String,
}

#[derive(Debug, Serialize)]
struct SearchResult {
    results: Vec<Function>,
}

#[derive(Debug, Serialize)]
struct Function {
    text: String,
    highlight: Vec<ValidExtent>,
}

#[get("/search?<query>")]
fn search(query: Query) -> JSON<SearchResult> {
    let ident_extents = IDENT_INDEX.get(&query.q).map_or(&[][..], Vec::as_slice);
    let matching_functions = strata::Containing::new(INDEX.functions.as_slice(), ident_extents);

    let mut results = Vec::new();
    for function_extent in matching_functions.iter_tau() {
        let function_text = &INDEX[function_extent];
        let function_extent_range = &[function_extent][..]; // TODO: impl Algebra for (u64, u64)?

        let highlighted_idents = strata::ContainedIn::new(ident_extents, function_extent_range);
        let offset_highlight_extents = highlighted_idents.iter_tau().map(|(s, e)| {
            // TODO: add an offset method?
            (s - function_extent.0, e - function_extent.0)
        }).collect();

        results.push(Function { text: function_text.to_owned(), highlight: offset_highlight_extents });
    }

    JSON(SearchResult { results })
}

fn main() {
    println!("Indexed {} idents", IDENT_INDEX.len());
    rocket::ignite().mount("/", routes![index, idents, search]).launch();
}
