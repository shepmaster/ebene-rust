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

fn parse_file(file: &str) {
    let mut pt = Point::new(file);
    let mut pm = Master::new();

    loop {
        let next_pt;

        let top_level = pm.alternate()
            .one(|pm| comment(pm, pt))
            .one(|pm| function(pm, pt))
            .one(|pm| p_trait(pm, pt))
            .one(|pm| whitespace(pm, pt))
            .finish();

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

        assert!(next_pt.offset > pt.offset);
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
    let (pt, generics) = try_parse!(function_generic_declarations(pm, pt));
    let (pt, args)     = try_parse!(function_arglist(pm, pt));
    let (pt, _)        = try_parse!(pm.optional(pt, whitespace));
    let (pt, wheres)   = try_parse!(function_where_clause(pm, pt));
    let (pt, _)        = try_parse!(pm.optional(pt, whitespace));
    let (pt, body)     = try_parse!(function_body(pm, pt));

    let function = Function {
        extent: (spt.offset, pt.offset),
        name: name,
        generics: generics,
        arguments: args,
        wheres: wheres,
        body: body
    };
    Progress::success(pt, TopLevel::Function(function))
}

fn ident<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    let spt = pt;
    let (pt, ex) = parse_until(pt, |c| {
        ['(', ')', ' ', '<', '>', ':', ','].contains(&c)
    });
    if pt.offset <= spt.offset {
        Progress::failure(pt, Error::IdentNotFound)
    } else {
        Progress::success(pt, ex)
    }
}

fn function_generic_declarations<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Vec<Generic>> {
    let (pt, _) = try_parse!(pt.consume_literal("<"));
    let (pt, decls) = try_parse!(pm.zero_or_more(pt, generic_declaration));
    let (pt, _) = try_parse!(pt.consume_literal(">"));
    Progress::success(pt, decls)
}

fn generic_declaration<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Generic> {
    ident(pm, pt)
}

fn function_arglist<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Vec<Argument>> {
    let (pt, _) = try_parse!(pt.consume_literal("("));
    let (pt, args) = try_parse!(pm.zero_or_more(pt, function_argument));
    let (pt, _) = try_parse!(pt.consume_literal(")"));
    Progress::success(pt, args)
}

fn function_argument<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Argument> {
    let (pt, name) = try_parse!(ident(pm, pt));
    let (pt, _) = try_parse!(pt.consume_literal(":"));
    let (pt, _) = try_parse!(pm.optional(pt, whitespace));
    let (pt, typ) = try_parse!(ident(pm, pt));
    let (pt, _) = try_parse!(pm.optional(pt, |_, pt| pt.consume_literal(",").map_err(Error::X)));
    Progress::success(pt, Argument { name: name, typ: typ })
}

fn function_where_clause<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Vec<Where>> {
    let (pt, _) = try_parse!(pt.consume_literal("where"));
    let (pt, _) = try_parse!(whitespace(pm, pt));
    pm.zero_or_more(pt, function_where)
}

fn function_where<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Where> {
    let (pt, name) = try_parse!(ident(pm, pt));
    let (pt, _) = try_parse!(pt.consume_literal(":"));
    let (pt, _) = try_parse!(pm.optional(pt, whitespace));
    let (pt, bounds) = try_parse!(ident(pm, pt));
    let (pt, _) = try_parse!(pm.optional(pt, |_, pt| pt.consume_literal(",").map_err(Error::X)));
    Progress::success(pt, Where { name: name, bounds: bounds })
}

fn function_body<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, FunctionBody> {
    let spt = pt;
    let (pt, _) = try_parse!(pt.consume_literal("{"));
    let (pt, exprs) = try_parse!(pm.zero_or_more(pt, expression));
    let (pt, _) = try_parse!(pt.consume_literal("}"));
    let body =  FunctionBody {
        extent: (spt.offset, pt.offset),
        expressions: exprs,
    };
    Progress::success(pt, body)
}

fn expression<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Expression> {
    let (pt, _) = try_parse!(pm.optional(pt, whitespace));
    let spt = pt;
    let (pt, kind) = try_parse!(macro_call(pm, pt));
    let ept = pt;
    let (pt, _) = try_parse!(pt.consume_literal(";"));
    let (pt, _) = try_parse!(pm.optional(pt, whitespace));
    Progress::success(pt, Expression { extent: (spt.offset, ept.offset), kind: kind })
}

fn macro_call<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ExpressionKind> {
    let (pt, name) = parse_until(pt, "!");
    let (pt, _) = try_parse!(pt.consume_literal("!"));
    let (pt, _) = try_parse!(pt.consume_literal("("));
    let (pt, args) = parse_until(pt, ")");
    let (pt, _) = try_parse!(pt.consume_literal(")"));
    Progress::success(pt, ExpressionKind::MacroCall { name: name, args: args })
}

fn p_trait<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TopLevel> {
    let spt = pt;
    let (pt, _) = try_parse!(pt.consume_literal("trait"));
    let (pt, _) = try_parse!(whitespace(pm, pt));
    let (pt, name) = try_parse!(ident(pm, pt));
    let (pt, _) = try_parse!(whitespace(pm, pt));
    let (pt, _) = try_parse!(pt.consume_literal("{}"));
    let t = Trait {
        extent: (spt.offset, pt.offset),
        name: name,
    };
    Progress::success(pt, TopLevel::Trait(t))
}

fn whitespace<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TopLevel> {
    let spt = pt;

    let (pt, _) = try_parse!(pm.zero_or_more(pt, |pm, pt| {
        pm.alternate()
            .one(|_| pt.consume_literal(" ").map_err(Error::X))
            .one(|_| pt.consume_literal("\t").map_err(Error::X))
            .one(|_| pt.consume_literal("\r").map_err(Error::X))
            .one(|_| pt.consume_literal("\n").map_err(Error::X))
            .finish()

    }));

    Progress::success(pt, TopLevel::Whitespace((spt.offset, pt.offset)))
}

fn main() {
    //parse_file(include_str!("data/hello_world.rs"));
    parse_file(include_str!("data/basic_generic.rs"));
}

// Goal:
// What methods take a generic argument that implements a trait `Foo`
