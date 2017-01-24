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

fn compile(q: StructuredQuery) -> Box<Algebra> {
    match q {
        StructuredQuery::Containing(lhs, rhs) => {
            let lhs = compile(*lhs);
            let rhs = compile(*rhs);
            Box::new(strata::Containing::new(lhs, rhs))
        }
        StructuredQuery::ContainedIn(lhs, rhs) => {
            let lhs = compile(*lhs);
            let rhs = compile(*rhs);
            Box::new(strata::ContainedIn::new(lhs, rhs))
        }
        StructuredQuery::Layer { name } => {
            Box::new(INDEX.layer_for(&name).expect("No layer!"))
        }
        StructuredQuery::Terminal { name, value } => {
            assert_eq!(name, "ident");
            Box::new(IDENT_INDEX.get(&value).map_or(&[][..], Vec::as_slice))
        }
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

#[derive(Debug, FromForm)]
struct Query {
    // renaming attributes?
    q: Option<JsonString<StructuredQuery>>,
    h: Option<JsonString<StructuredQuery>>,
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct JsonString<T>(T);

impl<'v, T> rocket::request::FromFormValue<'v> for JsonString<T>
    where T: serde::Deserialize,
{
    type Error = serde_json::Error;

    fn from_form_value(form_value: &'v str) -> Result<Self, Self::Error> {
        use rocket::http::uri::URI;
        let form_value = URI::percent_decode(form_value.as_bytes()).expect("NO UTF-8");
        serde_json::from_str(&form_value).map(JsonString)
    }
}

#[derive(Debug, Serialize, Deserialize)]
enum StructuredQuery {
    Containing(Box<StructuredQuery>, Box<StructuredQuery>),
    ContainedIn(Box<StructuredQuery>, Box<StructuredQuery>),
    Layer { name: String },
    Terminal { name: String, value: String },
}

#[derive(Debug, Default, Serialize)]
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
fn search(query: Query) -> JSON<SearchResults> {
    let q = match query.q {
        Some(q) => q.0,
        None => return JSON(SearchResults::default()),
    };

    let container_query = compile(q);
    let highlight_query = query.h.map(|h| compile(h.0));

    let results = container_query.iter_tau().map(|container_extent| {
        let container_text = &INDEX[container_extent];

        let highlight_extents = highlight_query.as_ref().map(|highlight_query| {
            let container_extent_range = &[container_extent][..]; // TODO: impl Algebra for (u64, u64)?;

            let this_result_highlights = strata::ContainedIn::new(highlight_query, container_extent_range);

            this_result_highlights.iter_tau().map(|ex| {
                offset_backwards(ex, container_extent.0)
            }).collect()
        }).unwrap_or_else(Default::default);

        SearchResult { text: container_text.to_owned(), highlight: highlight_extents }
    }).collect();

    JSON(SearchResults { results })
}

fn main() {
    println!("Indexed {} idents", IDENT_INDEX.len());
    rocket::ignite().mount("/", routes![index, idents, search]).launch();
}
