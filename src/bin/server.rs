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

#[macro_use]
extern crate quick_error;

use std::fs::File;
use std::collections::BTreeMap;

use strata::{Algebra, ValidExtent};

use rocket_contrib::JSON;

use quick_error::ResultExt;

quick_error! {
    #[derive(Debug, Clone, PartialEq)]
    enum Error {
        UnknownLayer(name: String)
        UnknownTerm(name: String)
    }
}

lazy_static! {
    static ref INDEX: Indexed = {
        let f = File::open("self.json").expect("no open file");
        serde_json::de::from_reader(f).expect("no parse")
    };
}

const IDENT_TERM_NAME: &'static str = "ident";

// This struct has mismatched "extent" types with the indexer...
#[derive(Debug, Default, Serialize, Deserialize)]
struct Indexed {
    source: String,
    layers: BTreeMap<String, Vec<strata::ValidExtent>>,
    terms: BTreeMap<String, BTreeMap<String, Vec<strata::ValidExtent>>>,
}

impl Indexed {
    fn layer_for(&self, name: &str) -> Option<&[strata::ValidExtent]> {
        self.layers.get(name).map(Vec::as_slice)
    }

    fn term_for(&self, name: &str, value: &str) -> Option<&[strata::ValidExtent]> {
        self.terms.get(name)
            .map(|x| {
                x.get(value).map_or(&[][..], Vec::as_slice)
            })
    }
}

impl std::ops::Index<strata::ValidExtent> for Indexed {
    type Output = str;

    fn index(&self, extent: ValidExtent) -> &str {
        &self.source[(extent.0 as usize)..(extent.1 as usize)]
    }
}

fn compile(q: StructuredQuery) -> Result<Box<Algebra>, Error> {
    match q {
        StructuredQuery::Nothing => {
            Ok(Box::new(strata::Empty))
        }
        StructuredQuery::Containing(lhs, rhs) => {
            let lhs = compile(*lhs)?;
            let rhs = compile(*rhs)?;
            Ok(Box::new(strata::Containing::new(lhs, rhs)))
        }
        StructuredQuery::ContainedIn(lhs, rhs) => {
            let lhs = compile(*lhs)?;
            let rhs = compile(*rhs)?;
            Ok(Box::new(strata::ContainedIn::new(lhs, rhs)))
        }
        StructuredQuery::NotContaining(lhs, rhs) => {
            let lhs = compile(*lhs)?;
            let rhs = compile(*rhs)?;
            Ok(Box::new(strata::NotContaining::new(lhs, rhs)))
        }
        StructuredQuery::NotContainedIn(lhs, rhs) => {
            let lhs = compile(*lhs)?;
            let rhs = compile(*rhs)?;
            Ok(Box::new(strata::NotContainedIn::new(lhs, rhs)))
        }
        StructuredQuery::BothOf(lhs, rhs) => {
            let lhs = compile(*lhs)?;
            let rhs = compile(*rhs)?;
            Ok(Box::new(strata::BothOf::new(lhs, rhs)))
        }
        StructuredQuery::OneOf(lhs, rhs) => {
            let lhs = compile(*lhs)?;
            let rhs = compile(*rhs)?;
            Ok(Box::new(strata::OneOf::new(lhs, rhs)))
        }
        StructuredQuery::FollowedBy(lhs, rhs) => {
            let lhs = compile(*lhs)?;
            let rhs = compile(*rhs)?;
            Ok(Box::new(strata::FollowedBy::new(lhs, rhs)))
        }
        StructuredQuery::Layer { name } => {
            INDEX.layer_for(&name)
                .map(|e| Box::new(e) as Box<Algebra>)
                .ok_or_else(|| Error::UnknownLayer(name))
        }
        StructuredQuery::Term { name, value } => {
            INDEX.term_for(&name, &value)
                .map(|e| Box::new(e) as Box<Algebra>)
                .ok_or_else(|| Error::UnknownTerm(name))
        }
    }
}

#[get("/dev/source")]
fn dev_source() -> JSON<String> {
    JSON(INDEX.source.clone())
}

#[get("/dev/layers")]
fn dev_layers() -> JSON<Vec<String>> {
    JSON(INDEX.layers.keys().map(Clone::clone).collect())
}

#[get("/dev/layers/<layer>")]
fn dev_layer(layer: &str) -> Option<JSON<Vec<ValidExtent>>> {
    INDEX.layers.get(layer).map(|l| JSON(l.to_owned()))
}

#[get("/dev/terms/")]
fn dev_terms() -> JSON<Vec<String>> {
    JSON(INDEX.terms.keys().map(Clone::clone).collect())
}

#[get("/dev/terms/<kind>")]
fn dev_terms_kinds(kind: &str) -> Option<JSON<Vec<String>>> {
    INDEX.terms.get(kind)
        .map(|k| JSON(k.keys().map(Clone::clone).collect()))
}

#[get("/dev/terms/<kind>/<term>")]
fn dev_terms_kind(kind: &str, term: &str) -> Option<JSON<Vec<ValidExtent>>> {
    INDEX.terms.get(kind)
        .and_then(|k| k.get(term))
        .map(|t| JSON(t.to_owned()))
}

#[derive(Debug, FromForm)]
struct Query {
    // renaming attributes?
    q: Option<JsonString<StructuredQuery>>,
    h: Option<JsonString<Vec<StructuredQuery>>>,
}

quick_error! {
    #[derive(Debug)]
    enum JsonStringError {
        NotUtf8(err: std::str::Utf8Error, val: String) {
            context(val: &'a str, err: std::str::Utf8Error) -> (err, val.to_owned())
        }
        NotDecodable(err: serde_json::Error, val: String) {
            context(val: String, err: serde_json::Error) -> (err, val)
        }
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct JsonString<T>(T);

impl<'v, T> rocket::request::FromFormValue<'v> for JsonString<T>
    where T: serde::Deserialize,
{
    type Error = JsonStringError;

    fn from_form_value(form_value: &'v str) -> Result<Self, Self::Error> {
        use rocket::http::uri::URI;
        let form_value = URI::percent_decode(form_value.as_bytes()).context(form_value)?;
        let form_value = serde_json::from_str(&form_value).context(form_value.into_owned())?;
        Ok(JsonString(form_value))
    }
}

#[derive(Debug, Serialize, Deserialize)]
enum StructuredQuery {
    Nothing,
    Containing(Box<StructuredQuery>, Box<StructuredQuery>),
    ContainedIn(Box<StructuredQuery>, Box<StructuredQuery>),
    NotContaining(Box<StructuredQuery>, Box<StructuredQuery>),
    NotContainedIn(Box<StructuredQuery>, Box<StructuredQuery>),
    BothOf(Box<StructuredQuery>, Box<StructuredQuery>),
    OneOf(Box<StructuredQuery>, Box<StructuredQuery>),
    FollowedBy(Box<StructuredQuery>, Box<StructuredQuery>),
    Layer { name: String },
    Term { name: String, value: String },
}

#[derive(Debug, Default, Serialize)]
struct SearchResults {
    results: Vec<SearchResult>,
}

#[derive(Debug, Serialize)]
struct SearchResult {
    text: String,
    highlights: Vec<Vec<ValidExtent>>,
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

    let container_query = compile(q).expect("can't compile query");
    let highlights = query.h.map_or_else(Vec::new, |h| h.0);
    let highlight_queries: Result<Vec<_>, _> = highlights.into_iter().map(compile).collect();
    let highlight_queries = highlight_queries.expect("Can't compile highlights");

    let results = container_query.iter_tau().map(|container_extent| {
        let container_text = &INDEX[container_extent];

        let highlights_extents = highlight_queries.iter().map(|highlight_query| {
            let container_extent_range = &[container_extent][..]; // TODO: impl Algebra for (u64, u64)?;

            let this_result_highlights = strata::ContainedIn::new(highlight_query, container_extent_range);

            this_result_highlights.iter_tau().map(|ex| {
                offset_backwards(ex, container_extent.0)
            }).collect()
        }).collect();

        SearchResult { text: container_text.to_owned(), highlights: highlights_extents }
    }).collect();

    JSON(SearchResults { results })
}

fn main() {
    println!("Indexed {:?} idents", INDEX.terms.get(IDENT_TERM_NAME).map(|s| s.len()));
    rocket::ignite().mount("/", routes![dev_source, dev_layers, dev_layer, dev_terms, dev_terms_kinds, dev_terms_kind, search]).launch();
}
