#![feature(pattern)]

#[macro_use]
extern crate peresil;

// define what you want to parse; likely a string
// create an error type
// definte type aliases
type Point<'s> = peresil::StringPoint<'s>;
type Master<'s> = peresil::ParseMaster<Point<'s>, Error>;
type Progress<'s, T> = peresil::Progress<Point<'s>, T, Error>;

// define an error type - emphasis on errors. Need to implement Recoverable (more to discuss.
#[derive(Debug)]
enum Error {
    IdentNotFound,
    X(()),
}

impl From<()> for Error {
    fn from(other: ()) -> Error {
        Error::X(other)
    }
}

impl peresil::Recoverable for Error {
    fn recoverable(&self) -> bool { true }
}

// Construct a point, initialize  the master. This is what stores errors
// todo: rename?

pub fn parse_rust_file(file: &str) {
    let mut pt = Point::new(file);
    let mut pm = Master::new();

    loop {
        let next_pt;

        let top_level = top_level(&mut pm, pt);

        match top_level.status {
            peresil::Status::Success(s) => {
                println!("Ok {:#?}", s);
                next_pt = top_level.point;
            },
            peresil::Status::Failure(e) => {
                println!("Err {:?}", e);
                println!(">>{}<<", &file[pt.offset..]);
                break;
            },
        }

        if next_pt.offset <= pt.offset {
            let end = std::cmp::min(pt.offset + 10, file.len());
            panic!("Could not make progress: {}...", &file[pt.offset..end]);
        }
        pt = next_pt;

        if pt.s.is_empty() { break }
    }

    // TODO: add `expect` to progress?
}

type Extent = (usize, usize);

#[derive(Debug)]
enum TopLevel {
    Comment(Extent),
    Function(Function),
    Trait(Trait),
    Whitespace(Extent),
}

#[derive(Debug)]
struct Function {
    extent: Extent,
    name: Extent,
    generics: Vec<Generic>,
    arguments: Vec<Argument>,
    wheres: Vec<Where>,
    body: FunctionBody,
}

//#[derive(Debug)]
type Generic = Extent;

#[derive(Debug)]
struct Argument {
    name: Extent,
    typ: Extent,
}

#[derive(Debug)]
struct Where {
    name: Extent,
    bounds: Extent,
}

#[derive(Debug)]
struct FunctionBody {
    extent: Extent,
    expressions: Vec<Expression>,
}

#[derive(Debug)]
struct Expression {
    extent: Extent,
    kind: ExpressionKind,
}

#[derive(Debug)]
enum ExpressionKind {
    MacroCall { name: Extent, args: Extent },
}

#[derive(Debug)]
struct Trait {
    extent: Extent,
    name: Extent,
}

// extract fn to library?
fn parse_until<'s, P>(pt: Point<'s>, p: P) -> (Point<'s>, Extent)
    where P: std::str::pattern::Pattern<'s>
{
    let end = pt.s.find(p).unwrap_or(pt.s.len());
    let k = &pt.s[end..];
    (Point { s: k, offset: pt.offset + end }, (pt.offset, pt.offset + end))
}

// TODO: consider passing in `pt` to alternate to pass it back to
// closure allow for nice composition
fn top_level<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TopLevel> {
    pm.alternate()
        .one(|pm| comment(pm, pt))
        .one(|pm| function(pm, pt))
        .one(|pm| p_trait(pm, pt))
        .one(|pm| whitespace(pm, pt))
        .finish()
}

fn comment<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TopLevel> {
    let (pt, _) = try_parse!(pt.consume_literal("//"));
    let spt = pt;
    let (pt, _) = parse_until(pt, "\n");

    Progress::success(pt, TopLevel::Comment((spt.offset, pt.offset)))
}

fn function<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TopLevel> {
    let spt = pt;

    let (pt, _)        = try_parse!(pt.consume_literal("fn"));
    let (pt, _)        = try_parse!(pm.optional(pt, whitespace));
    let (pt, name)     = try_parse!(ident(pm, pt));
    let (pt, generics) = try_parse!(pm.optional(pt, function_generic_declarations));
    let (pt, args)     = try_parse!(function_arglist(pm, pt));
    let (pt, _)        = try_parse!(pm.optional(pt, whitespace));
    let (pt, wheres)   = try_parse!(pm.optional(pt, function_where_clause));
    let (pt, _)        = try_parse!(pm.optional(pt, whitespace));
    let (pt, body)     = try_parse!(function_body(pm, pt));

    Progress::success(pt, TopLevel::Function(Function {
        extent: (spt.offset, pt.offset),
        name: name,
        generics: generics.unwrap_or_else(Vec::new),
        arguments: args,
        wheres: wheres.unwrap_or_else(Vec::new),
        body: body
    }))
}

fn ident<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    let spt = pt;
    let (pt, ex) = parse_until(pt, |c| {
        ['!', '(', ')', ' ', '<', '>', ':', ','].contains(&c)
    });
    if pt.offset <= spt.offset {
        Progress::failure(pt, Error::IdentNotFound)
    } else {
        Progress::success(pt, ex)
    }
}

fn function_generic_declarations<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Vec<Generic>> {
    let (pt, _)     = try_parse!(pt.consume_literal("<"));
    let (pt, decls) = try_parse!(one_or_more(pm, pt, generic_declaration));
    let (pt, _)     = try_parse!(pt.consume_literal(">"));

    Progress::success(pt, decls)
}

fn generic_declaration<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Generic> {
    ident(pm, pt)
}

fn function_arglist<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Vec<Argument>> {
    let (pt, _)    = try_parse!(pt.consume_literal("("));
    let (pt, args) = try_parse!(pm.zero_or_more(pt, function_argument));
    let (pt, _)    = try_parse!(pt.consume_literal(")"));

    Progress::success(pt, args)
}

fn comma<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, &'s str> {
    pt.consume_literal(",").map_err(Error::X)
}

fn function_argument<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Argument> {
    let (pt, name) = try_parse!(ident(pm, pt));
    let (pt, _)    = try_parse!(pt.consume_literal(":"));
    let (pt, _)    = try_parse!(pm.optional(pt, whitespace));
    let (pt, typ)  = try_parse!(ident(pm, pt));
    let (pt, _)    = try_parse!(pm.optional(pt, comma));

    Progress::success(pt, Argument { name: name, typ: typ })
}

fn function_where_clause<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Vec<Where>> {
    let (pt, _) = try_parse!(pt.consume_literal("where"));
    let (pt, _) = try_parse!(whitespace(pm, pt));

    one_or_more(pm, pt, function_where)
}

fn function_where<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Where> {
    let (pt, name)   = try_parse!(ident(pm, pt));
    let (pt, _)      = try_parse!(pt.consume_literal(":"));
    let (pt, _)      = try_parse!(pm.optional(pt, whitespace));
    let (pt, bounds) = try_parse!(ident(pm, pt));
    let (pt, _)      = try_parse!(pm.optional(pt, comma));

    Progress::success(pt, Where { name: name, bounds: bounds })
}

fn function_body<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, FunctionBody> {
    let spt = pt;
    let (pt, _)     = try_parse!(pt.consume_literal("{"));
    let (pt, exprs) = try_parse!(pm.zero_or_more(pt, expression));
    let (pt, _)     = try_parse!(pt.consume_literal("}"));

    Progress::success(pt, FunctionBody {
        extent: (spt.offset, pt.offset),
        expressions: exprs,
    })
}

fn expression<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Expression> {
    let (pt, _)    = try_parse!(pm.optional(pt, whitespace));
    let spt        = pt;
    let (pt, kind) = try_parse!(macro_call(pm, pt));
    let ept        = pt;
    let (pt, _)    = try_parse!(pt.consume_literal(";"));
    let (pt, _)    = try_parse!(pm.optional(pt, whitespace));

    Progress::success(pt, Expression { extent: (spt.offset, ept.offset), kind: kind })
}

fn macro_call<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ExpressionKind> {
    let (pt, name) = try_parse!(ident(pm, pt));
    let (pt, _)    = try_parse!(pt.consume_literal("!"));
    let (pt, _)    = try_parse!(pt.consume_literal("("));
    let (pt, args) = parse_until(pt, ")");
    let (pt, _)    = try_parse!(pt.consume_literal(")"));

    Progress::success(pt, ExpressionKind::MacroCall { name: name, args: args })
}

fn p_trait<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TopLevel> {
    let spt        = pt;
    let (pt, _)    = try_parse!(pt.consume_literal("trait"));
    let (pt, _)    = try_parse!(whitespace(pm, pt));
    let (pt, name) = try_parse!(ident(pm, pt));
    let (pt, _)    = try_parse!(whitespace(pm, pt));
    let (pt, _)    = try_parse!(pt.consume_literal("{}"));

    Progress::success(pt, TopLevel::Trait(Trait {
        extent: (spt.offset, pt.offset),
        name: name,
    }))
}

fn whitespace<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TopLevel> {
    let spt = pt;

    let (pt, _) = try_parse!(one_or_more(pm, pt, |pm, pt| {
        pm.alternate()
            .one(|_| pt.consume_literal(" ").map_err(Error::X))
            .one(|_| pt.consume_literal("\t").map_err(Error::X))
            .one(|_| pt.consume_literal("\r").map_err(Error::X))
            .one(|_| pt.consume_literal("\n").map_err(Error::X))
            .finish()

    }));

    Progress::success(pt, TopLevel::Whitespace((spt.offset, pt.offset)))
}

fn one_or_more<'s, F, T>(pm: &mut Master<'s>, pt: Point<'s>, mut f: F) -> Progress<'s, Vec<T>>
    where F: FnMut(&mut Master<'s>, Point<'s>) -> Progress<'s, T>,
{
    let (pt, head) = try_parse!(f(pm, pt));
    let (pt, mut tail) = try_parse!(pm.zero_or_more(pt, f));

    tail.insert(0, head);
    Progress::success(pt, tail)
}
