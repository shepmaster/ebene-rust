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
        let top_level = pm.alternate()
            .one(|pm| comment(pm, pt))
            .one(|pm| function(pm, pt))
            .one(|pm| whitespace(pm, pt))
            .finish();

        match top_level.status {
            peresil::Status::Success(s) => {
                println!("Ok {:#?}", s);
                pt = top_level.point;
            },
            peresil::Status::Failure(e) => {
                println!("Err {:?}", e);
                println!(">>{}<<", &file[pt.offset..]);
                break;
            },
        }

        if pt.s.is_empty() { break }
    }

    // TODO: add `expect` to progress?
}

type Extent = (usize, usize);

#[derive(Debug)]
enum TopLevel {
    Comment(Extent),
    Function(Function),
    Whitespace(Extent),
}

#[derive(Debug)]
struct Function {
    extent: Extent,
    name: Extent,
    body: FunctionBody,
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

// extract fn to library?
fn parse_until<'s>(pt: Point<'s>, s: &str) -> (Point<'s>, Extent) {
    let end = pt.s.find(s).unwrap_or(pt.s.len());
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
    let (pt, _) = try_parse!(pt.consume_literal("fn"));
    let (pt, _) = try_parse!(pt.consume_literal(" "));
    let (pt, name) = try_parse!(ident(pm, pt));
    let (pt, _) = try_parse!(function_arglist(pm, pt));
    let (pt, _) = try_parse!(pt.consume_literal(" ")); // optional?
    let (pt, body) = try_parse!(function_body(pm, pt));
    let function = Function {
        extent: (spt.offset, pt.offset),
        name: name,
        body: body
    };
    Progress::success(pt, TopLevel::Function(function))
}

fn ident<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    let (pt, ex) = parse_until(pt, "(");
    Progress::success(pt, ex)
}

fn function_arglist<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    let (pt, _) = try_parse!(pt.consume_literal("()"));
    Progress::success(pt, (0, 0))
}

fn function_body<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, FunctionBody> {
    let spt = pt;
    let (pt, _) = try_parse!(spt.consume_literal("{"));
    let (pt, exprs) = try_parse!(pm.zero_or_more(pt, |pm, pt| expression(pm, pt)));
    let (pt, _) = try_parse!(pt.consume_literal("}"));
    let body =  FunctionBody {
        extent: (spt.offset, pt.offset),
        expressions: exprs,
    };
    Progress::success(pt, body)
}

fn expression<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Expression> {
    let (pt, _) = try_parse!(pm.optional(pt, |pm, pt| whitespace(pm, pt)));
    let spt = pt;
    let (pt, kind) = try_parse!(macro_call(pm, pt));//parse_until(pt, ";");
    let ept = pt;
    let (pt, _) = try_parse!(pt.consume_literal(";"));
    let (pt, _) = try_parse!(pm.optional(pt, |pm, pt| whitespace(pm, pt)));
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
    parse_file(include_str!("data/hello_world.rs"));
}

// Goal:
// What methods take a generic argument that implements a trait `Foo`
