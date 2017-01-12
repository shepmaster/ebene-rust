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
        let function = pm.alternate()
            .one(|pm| comment(pm, pt))
            .one(|pm| function(pm, pt))
            .one(|pm| whitespace(pm, pt))
            .finish();

        match function.status {
            peresil::Status::Success(s) => {
                println!("Ok {:?}", s);
                println!("[{}]", &file[s.0..s.1]);
                pt = function.point;
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
struct Function {
    name: Extent,
    body: Extent,
}

// struct Zoo<'s> {
//     pm: Master<'s>,
// }

// extract fn to library?
fn parse_until<'s>(pt: Point<'s>, s: &str) -> Point<'s> {
    let end = pt.s.find(s).unwrap_or(pt.s.len());
    let k = &pt.s[end..];
    Point { s: k, offset: pt.offset + end }
}

//impl<'s> Zoo<'s> {

fn comment<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    let (pt, _) = try_parse!(pt.consume_literal("//"));
    let ept = parse_until(pt, "\n");
    Progress::success(ept, (pt.offset, ept.offset))
}

fn function<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    let (pt, _) = try_parse!(pt.consume_literal("fn"));
    let (pt, _) = try_parse!(pt.consume_literal(" "));
    let (pt, name) = try_parse!(ident(pm, pt));
    let (pt, _) = try_parse!(function_arglist(pm, pt));
    let (pt, _) = try_parse!(pt.consume_literal(" ")); // optional?
    let (pt, body) = try_parse!(function_body(pm, pt));
    println!("{:?}", Function { name: name, body: body });
    Progress::success(pt, (0, 0))
}

fn ident<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    let ept = parse_until(pt, "(");
    Progress::success(ept, (pt.offset, ept.offset))
}

fn function_arglist<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    let (pt, _) = try_parse!(pt.consume_literal("()"));
    Progress::success(pt, (0, 0))
}

fn function_body<'s>(_pm: &mut Master<'s>, spt: Point<'s>) -> Progress<'s, Extent> {
    let (pt, _) = try_parse!(spt.consume_literal("{"));
    let pt = parse_until(pt, "}");
    let (ept, _) = try_parse!(pt.consume_literal("}"));
    Progress::success(ept, (spt.offset, ept.offset))
}

fn whitespace<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    let (ept, _) = try_parse!(pt.consume_literal("\n"));
    Progress::success(ept, (pt.offset, ept.offset))
}

fn main() {
    parse_file(include_str!("data/hello_world.rs"));
}
