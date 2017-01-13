extern crate strata_rs;

fn main() {
  //  strata_rs::parse_rust_file(include_str!("data/hello_world.rs"));
    strata_rs::parse_rust_file(include_str!("data/basic_generic.rs"));

//    strata_rs::parse_rust_file(include_str!("../src/lib.rs"));
}

// Goal:
// What methods take a generic argument that implements a trait `Foo`
