# Ebene-Rust

Ebene-Rust is a unique way of exploring Rust code, aiming to answer
questions like:

- Which methods take a generic argument that implements the trait `Foo`?
- Do any structs contain a field called `data`?
- What statements come after a "TODO" comment?
- Are weird statements like `let Single { y }: Single<&'static u32>;` ever used?

## How do I use it?

Right now, this project is very much in an alpha state. That's the
polite way of saying "it works on my machine, and probably no
other". I'm hoping to get it into a shape where it at least compiles
in CI soon.

## Components

Ebene-Rust is a combination of many pieces of open source
technology. Some selected pieces include:

### Ebene

[Ebene][] is the underlying search technology that powers the ability
to perform queries based on the structure of the data. These queries
are different from the traditional search engine inverted index and
are based around manipulating ranges of the indexed text known as
"extents".

The Ebene crate works well on stable Rust.

[Ebene]: https://github.com/shepmaster/ebene

### Fuzzy Pickles

[Fuzzy Pickles][] is a Rust parser written in Rust, constructed with
the goal of expressing the level of detail needed to power the
underlying search technology.

Fuzzy Pickles works well on stable Rust.

[Fuzzy Pickles]: https://github.com/shepmaster/fuzzy-pickles

### Web UI

Although the command line is a wonderful tool, building up the
complicated queries and showing the highlighted results works best in
a medium with more fidelity, such as the web. A [Rocket][] backend
powers a [React][] and [Redux][] frontend.

Rocket requires **nightly** Rust.

[Rocket]: https://rocket.rs/
[React]: https://facebook.github.io/react/
[Redux]: http://redux.js.org/

## Contribution opportunities

A project always has need for help from interested people!

### Introductory opportunities 🌟

These are things that anyone should be able to help with!

- Suggest some queries that might be answerable given the structure of
  a Rust file. Ones that you have had in the Real World are preferred!
- Try to run the indexer and server on your own files.

### Intermediate opportunities 🌟🌟🌟

These might require diving into the code a bit and adding new code or
changing existing code.

- To be determined!

Please open up an issue to begin a dialog before starting if a feature
seems like it will require more than just a straight-forward addition!

### Advanced opportunities 🌟🌟🌟🌟🌟

These are intense feature requests that probably require a good amount
of effort and *definitely* should be discussed in an issue before
beginning.

- To be determined!

## License

ebene-rust is distributed under the terms of both the MIT license and
the Apache License (Version 2.0).

## Authors

This crate was created by Jake Goulding of [Integer 32][].

[Integer 32]: http://www.integer32.com/
