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
    enums: Vec<strata::ValidExtent>,
    structs: Vec<strata::ValidExtent>,
}

impl Indexed {
    fn layer_for(&self, name: &str) -> Result<&[strata::ValidExtent], ()> {
        match name {
            "function" => Ok(&self.functions),
            "ident" => Ok(&self.idents),
            "enum" => Ok(&self.enums),
            "struct" => Ok(&self.structs),
            _ => Err(()),
        }
    }
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
    q: Option<String>,
    within: String,
}

#[derive(Debug, Serialize)]
struct SearchResults {
    results: Vec<SearchResult>,
}

#[derive(Debug, Serialize)]
struct SearchResult {
    text: String,
    highlight: Vec<ValidExtent>,
}

fn offset_backwards(extent: ValidExtent, offset: u64) -> ValidExtent {
    (extent.0 - offset, extent.1 - offset)
}

#[get("/search?<query>")]
fn search(mut query: Query) -> JSON<SearchResults> {
    if query.q.as_ref().map_or(true, String::is_empty) {
        query.q = None;
    }

    // TODO: Figure out deduplication;
    let results = match query.q {
        Some(ref q) => {
            let ident_extents = IDENT_INDEX.get(q).map_or(&[][..], Vec::as_slice);

            let container_extents = INDEX.layer_for(&query.within).expect("Unknown layer");

            let matching_container = strata::Containing::new(container_extents, ident_extents);

            matching_container.iter_tau().map(|container_extent| {
                let container_text = &INDEX[container_extent];

                let container_extent_range = &[container_extent][..]; // TODO: impl Algebra for (u64, u64)?
                let highlighted_idents = strata::ContainedIn::new(ident_extents, container_extent_range);
                let offset_highlight_extents = highlighted_idents.iter_tau().map(|ex| {
                    offset_backwards(ex, container_extent.0)
                }).collect();

                SearchResult { text: container_text.to_owned(), highlight: offset_highlight_extents }
            }).collect()
        }
        None => {
            let container_extents = INDEX.layer_for(&query.within).expect("Unknown layer");
            let matching_container = container_extents;
            matching_container.iter_tau().map(|container_extent| {
                let container_text = &INDEX[container_extent];

                SearchResult { text: container_text.to_owned(), highlight: Vec::new() }
            }).collect()
        }
    };

    JSON(SearchResults { results })
}

fn main() {
    println!("Indexed {} idents", IDENT_INDEX.len());
    rocket::ignite().mount("/", routes![index, idents, search]).launch();
}
