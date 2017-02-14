# Strata-Rust

Strata-Rust is a unique way of exploring Rust code, aiming to answer
questions like:

- Which methods take a generic argument that implements the trait `Foo`?
- Do any structs contain a field called `data`?
- What statements come after a "TODO" comment?

## How do I use it?

Right now, this project is very much in an alpha state. That's the
polite way of saying "it works on my machine, and probably no
other". I'm hoping to get it into a shape where it at least compiles
in CI soon.

## Components

Strata-Rust is a combination of many pieces of open source
technology. Some selected pieces include:

### Strata

[Strata][] is the underlying search technology that powers the ability
to perform queries based on the structure of the data. These queries
are different from the traditional search engine inverted index and
are based around manipulating ranges of the indexed text known as
"extents".

The Strata crate works well on stable Rust.

[Strata]: https://github.com/shepmaster/strata

### Untitled Rust Parser

While there are many high-quality Rust parsers available, a slightly
different kind of parser was needed to provide the detail needed for
the extents. To that end, a new parser was built using the [Peresil][]
parsing toolkit. The parser aims to:

- be a low-level parser of Rust source code, providing unshackled
  access to the direct parsing structures.
- provide an easier-to-use visitor interface that allows quickly
  gathering information about Rust code.
- provide reasonable quality errors about the parsing.
- parse all syntactically-valid Rust code.

It has anti-goals as well! The parser does not:

- attempt to perform *semantic* analysis of Rust code.
- guarantee to reject all syntactically-invalid code.

It is expected that this parser will eventually grow a name and be
extracted from this project. Currently it is of alpha quality and we
know there is valid syntax that cannot be parsed.

While Peresil works with stable Rust, our usage of it enables some
**nightly-only** features.

[Peresil]: https://github.com/shepmaster/peresil

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
- Run the parser against your own Rust file, or a particularly
  interesting file you are aware of.
- Narrow down a file that fails to parse to construct a test case.

### Intermediate opportunities 🌟🌟🌟

These might require diving into the code a bit and adding new code or
changing existing code.

- Enhance the parser to recognize code that is currently not recognizable.
- Verify that a piece of code is parsed correctly.

Please open up an issue to begin a dialog before starting if a feature
seems like it will require more than just a straight-forward addition!

### Advanced opportunities 🌟🌟🌟🌟🌟

These are intense feature requests that probably require a good amount
of effort and *definitely* should be discussed in an issue before
beginning.

- Perform macro expansion

## License

strata-rust is distributed under the terms of both the MIT license and
the Apache License (Version 2.0).

## Authors

This crate was created by Jake Goulding of [Integer 32][].

[Integer 32]: http://www.integer32.com/
