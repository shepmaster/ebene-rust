#![feature(field_init_shorthand)]
#![feature(conservative_impl_trait)]
#![feature(pattern)]

#[macro_use]
extern crate peresil;

use std::collections::BTreeSet;

// define what you want to parse; likely a string
// create an error type
// definte type aliases
type Point<'s> = peresil::StringPoint<'s>;
type Master<'s> = peresil::ParseMaster<Point<'s>, Error>;
type Progress<'s, T> = peresil::Progress<Point<'s>, T, Error>;

// define an error type - emphasis on errors. Need to implement Recoverable (more to discuss.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Error {
    Literal(&'static str),
    IdentNotFound,
    UnterminatedRawString,
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
        let top_level = pm.finish(top_level);

        match top_level.status {
            peresil::Status::Success(s) => {
                println!("Ok {:#?}", s);
                next_pt = top_level.point;
            },
            peresil::Status::Failure(e) => {
                println!("Err @ {}: {:?}", top_level.point.offset, e.into_iter().collect::<BTreeSet<_>>());
                println!(">>{}<<", &file[top_level.point.offset..]);
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

// TODO: enum variants track whole extent, enum delegates

type Extent = (usize, usize);

#[derive(Debug)]
enum TopLevel {
    Function(Function),
    MacroRules(MacroRules),
    Struct(Struct),
    Enum(Enum),
    Trait(Trait),
    Impl(Impl),
    Attribute(Attribute),
    ExternCrate(Crate),
    Use(Use),
    TypeAlias(TypeAlias),
    Module(Module),
    Whitespace(Vec<Whitespace>),
}

impl TopLevel {
    #[allow(dead_code)]
    fn extent(&self) -> Extent {
        match *self {
            TopLevel::Function(Function { extent, .. })     |
            TopLevel::MacroRules(MacroRules { extent, .. }) |
            TopLevel::Struct(Struct { extent, .. })         |
            TopLevel::Enum(Enum { extent, .. })             |
            TopLevel::Trait(Trait { extent, .. })           |
            TopLevel::Impl(Impl { extent, .. })             |
            TopLevel::Attribute(extent)                     |
            TopLevel::ExternCrate(Crate { extent, .. })     |
            TopLevel::Use(Use { extent, .. })               |
            TopLevel::TypeAlias(TypeAlias { extent, .. })   |
            TopLevel::Module(Module { extent, .. })         => extent,
            TopLevel::Whitespace(..)                        => unimplemented!(),
        }
    }
}

type Attribute = Extent;
type Lifetime = Extent;

#[derive(Debug)]
enum Whitespace {
    Comment(Comment),
    Whitespace(Extent),
}

#[derive(Debug)]
struct Comment {
    extent: Extent,
    text: Extent,
}

#[derive(Debug)]
struct Use {
    extent: Extent,
    name: Extent,
}

#[derive(Debug)]
struct Function {
    extent: Extent,
    header: FunctionHeader,
    body: Block,
}

#[derive(Debug)]
struct FunctionHeader {
    extent: Extent,
    visibility: Option<Extent>,
    name: Extent,
    generics: Option<GenericDeclarations>,
    arguments: Vec<Argument>,
    return_type: Option<Type>,
    wheres: Vec<Where>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug)]
struct GenericDeclarations {
    lifetimes: Vec<Lifetime>,
    types: Vec<Generic>,
}

#[derive(Debug)]
struct MacroRules {
    extent: Extent,
    name: Ident,
    body: Extent,
}

//#[derive(Debug)]
type Generic = Extent;

type Type = Extent;
type Ident = Extent;

fn ex(start: Point, end: Point) -> Extent {
    let ex = (start.offset, end.offset);
    assert!(ex.1 >= ex.0, "{} does not come before {}", ex.1, ex.0);
    ex
}

#[derive(Debug)]
struct Struct {
    extent: Extent,
    name: Extent,
    fields: Vec<StructField>,
}

#[derive(Debug)]
struct StructField {
    extent: Extent,
    name: Extent,
    typ: Type,
}

#[derive(Debug)]
struct Enum {
    extent: Extent,
    name: Extent,
    variants: Vec<EnumVariant>,
}

#[derive(Debug)]
struct EnumVariant {
    extent: Extent,
    name: Extent,
    body: Option<EnumVariantBody>,
}

#[derive(Debug)]
enum EnumVariantBody {
    Tuple(Extent),
    Struct(Extent),
}

#[derive(Debug)]
enum Argument {
    SelfArgument,
    Named { name: Extent, typ: Type }
}

#[derive(Debug)]
struct Where {
    extent: Extent,
    name: Type,
    bounds: Vec<Extent>,
}

#[derive(Debug)]
struct Block {
    extent: Extent,
    statements: Vec<Statement>,
    expression: Option<Expression>,
}

#[derive(Debug)]
enum Statement {
    Explicit(Expression),
    Implicit(Expression),
    Use(Use),
}

impl Statement {
    #[allow(dead_code)]
    fn extent(&self) -> Extent {
        use Statement::*;
        match *self {
            Explicit(ref e) |
            Implicit(ref e) => e.extent,
            Use(ref u) => u.extent,
        }
    }

    #[allow(dead_code)]
    fn explicit(self) -> Option<Expression> {
        match self {
            Statement::Explicit(e) => Some(e),
            _ => None,
        }
    }

    #[allow(dead_code)]
    fn implicit(self) -> Option<Expression> {
        match self {
            Statement::Implicit(e) => Some(e),
            _ => None,
        }
    }

    fn is_implicit(&self) -> bool {
        match *self {
            Statement::Implicit(..) => true,
            _ => false
        }
    }
}

#[derive(Debug)]
struct Expression {
    extent: Extent,
    kind: ExpressionKind,
}

#[derive(Debug)]
enum ExpressionKind {
    MacroCall(MacroCall),
    Call(Call),
    Let(Let),
    Assign(Assign),
    Tuple(Tuple),
    FieldAccess(FieldAccess),
    Value(Value),
    Block(Box<Block>),
    FunctionCall(FunctionCall),
    MethodCall(MethodCall),
    ForLoop(ForLoop),
    Loop(Loop),
    Binary(Binary),
    If(If),
    Match(Match),
    Range(Range),
    Array(Array),
    Character(Character),
    String(String),
    Slice(Slice),
    Closure(Closure),
    Return(Return),
}

#[derive(Debug)]
struct MacroCall {
    name: Extent,
    args: Extent,
}

#[derive(Debug)]
struct Let {
    pattern: Pattern,
    typ: Option<Type>,
    value: Option<Box<Expression>>,
}

#[derive(Debug)]
struct Assign {
    name: Extent,
    value: Box<Expression>,
}

#[derive(Debug)]
struct Tuple {
    members: Vec<Expression>,
}

#[derive(Debug)]
struct FieldAccess {
    value: Box<Expression>,
    field: Extent
}

#[derive(Debug)]
struct Value {
    name: Extent,
    literal: Option<Vec<StructLiteralField>>,
}

#[derive(Debug)]
struct StructLiteralField {
    name: Ident,
    value: Expression,
}

// TODO: Can we roll up function and method call into this?
#[derive(Debug)]
struct Call {
    target: Box<Expression>,
    args: Vec<Expression>,
}

#[derive(Debug)]
struct FunctionCall {
    name: Extent,
    args: Vec<Expression>,
}

#[derive(Debug)]
struct MethodCall {
    receiver: Box<Expression>,
    name: Extent,
    turbofish: Option<Extent>,
    args: Vec<Expression>,
}

#[derive(Debug)]
struct ForLoop {
    pattern: Pattern,
    iter: Box<Expression>,
    body: Box<Block>,
}

#[derive(Debug)]
struct Loop {
    body: Box<Block>,
}

#[derive(Debug)]
struct Binary {
    op: Extent,
    lhs: Box<Expression>,
    rhs: Box<Expression>,
}

#[derive(Debug)]
struct If {
    extent: Extent,
    condition: Box<Expression>,
    body: Box<Block>,
    more: Vec<If>,
    else_body: Option<Box<Block>>,
}

#[derive(Debug)]
struct Match {
    head: Box<Expression>,
    arms: Vec<MatchArm>,
}

#[derive(Debug)]
struct MatchArm {
    extent: Extent,
    pattern: Vec<Pattern>,
    body: Expression,
}

#[derive(Debug)]
struct Range {
    lhs: Option<Box<Expression>>,
    rhs: Option<Box<Expression>>,
}

#[derive(Debug)]
struct Array {
    members: Vec<Expression>,
}

#[derive(Debug)]
struct Character {
    value: Extent,
}

#[derive(Debug)]
struct String {
    value: Extent,
}

#[derive(Debug)]
struct Slice {
    target: Box<Expression>,
    range: Box<Expression>,
}

#[derive(Debug)]
struct Closure {
    is_move: bool,
    args: Vec<ClosureArg>,
    body: Box<Expression>,
}

#[derive(Debug)]
struct ClosureArg {
    name: Ident,
    typ: Option<Type>,
}

#[derive(Debug)]
struct Return {
    extent: Extent,
    value: Box<Expression>,
}

#[derive(Debug)]
enum ExpressionTail {
    Binary { op: Extent, rhs: Box<Expression> },
    FieldAccess { field: Extent },
    Call { args: Vec<Expression> },
    MethodCall { name: Extent, turbofish: Option<Extent>, args: Vec<Expression> },
    Range { rhs: Option<Box<Expression>> },
    Slice { range: Box<Expression> },
}

#[derive(Debug)]
enum Pattern {
    // TODO: split into ident and enumtuple
    Ident { extent: Extent, ident: Extent, tuple: Vec<Pattern> },
    Struct { extent: Extent, name: Extent, fields: Vec<PatternStructField>, wildcard: bool },
    Tuple { extent: Extent, members: Vec<Pattern> },
    Wildcard { extent: Extent },
    Character { extent: Extent, value: Character },
}

impl Pattern {
    #[allow(dead_code)]
    fn extent(&self) -> Extent {
        use Pattern::*;
        match *self {
            Ident { extent, .. } |
            Tuple { extent, .. } |
            Struct { extent, .. } |
            Wildcard { extent, .. } |
            Character { extent, .. } => extent
        }
    }
}

#[derive(Debug)]
struct PatternStructField {
    name: Extent,
    pattern: Pattern,
}

#[derive(Debug)]
struct Trait {
    extent: Extent,
    name: Extent,
    generics: Option<GenericDeclarations>,
    members: Vec<TraitMember>,
}

#[derive(Debug)]
enum TraitMember {
    Function(TraitImplFunction),
    Attribute(Attribute),
    Whitespace(Vec<Whitespace>),
}

#[derive(Debug)]
struct TraitImplFunction {
    extent: Extent,
    header: FunctionHeader,
    body: Option<Block>,
}

#[derive(Debug)]
struct Impl {
    extent: Extent,
    generics: Option<GenericDeclarations>,
    trait_name: Option<Type>,
    type_name: Type,
    body: Vec<ImplMember>,
}

#[derive(Debug)]
enum ImplMember {
    Function(ImplFunction),
    Attribute(Attribute),
    Whitespace(Vec<Whitespace>),
}

#[derive(Debug)]
struct ImplFunction {
    extent: Extent,
    header: FunctionHeader,
    body: Block,
}

#[derive(Debug)]
struct Crate {
    extent: Extent,
    name: Extent,
}

#[derive(Debug)]
struct TypeAlias {
    extent: Extent,
    name: Type,
    defn: Type,
}

#[derive(Debug)]
struct Module {
    extent: Extent,
    name: Ident,
    body: Vec<TopLevel>,
}

// TODO: extract to peresil?
fn parse_until<'s, P>(pt: Point<'s>, p: P) -> (Point<'s>, Extent)
    where P: std::str::pattern::Pattern<'s>
{
    let end = pt.s.find(p).unwrap_or(pt.s.len());
    let k = &pt.s[end..];
    (Point { s: k, offset: pt.offset + end }, (pt.offset, pt.offset + end))
}

// TODO: extract to peresil?
fn parse_until2<'s, P>(p: P) -> impl Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, Extent>
    where P: std::str::pattern::Pattern<'s> + Clone // TODO: eww clone
{
    move |_, pt| {
        let spt = pt;
        let end = pt.s.find(p.clone()).unwrap_or(pt.s.len());
        let k = &pt.s[end..];
        let pt = Point { s: k, offset: pt.offset + end };

        Progress::success(pt, ex(spt, pt))
    }
}

// TODO: extract to peresil?
fn parse_nested_until<'s>(open: char, close: char) -> impl Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, Extent> {
    move |_, pt| {
        let mut depth: usize = 0;
        let spt = pt;

        let val = |len| {
            let pt = Point { s: &pt.s[len..], offset: pt.offset + len };
            Progress::success(pt, ex(spt, pt))
        };

        for (i, c) in pt.s.char_indices() {
            if c == close && depth == 0 {
                return val(i);
            } else if c == close {
                depth -= 1;
            } else if c == open {
                depth += 1;
            }
        }
        val(pt.s.len())
    }
}

// TODO: extract to peresil
fn one_or_more<'s, F, T>(f: F) -> impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Vec<T>>
    where F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>,
{
    move |pm, pt| {
        let (pt, head) = try_parse!(f(pm, pt));
        let existing = vec![head];
        let (pt, tail) = try_parse!(zero_or_more_append(existing, f)(pm, pt));

        Progress::success(pt, tail)
    }
}

// TODO: extract to peresil
// alternate syntax: `foo: parser;`?
macro_rules! sequence {
    ($pm:expr, $pt:expr, {let $x:pat = $parser:expr; $($rest:tt)*}, $creator:expr) => {{
        let (pt, $x) = try_parse!($parser($pm, $pt));
        sequence!($pm, pt, {$($rest)*}, $creator)
    }};
    ($pm:expr, $pt:expr, {$x:pat = $parser:expr; $($rest:tt)*}, $creator:expr) => {{
        let (pt, $x) = try_parse!($parser($pm, $pt));
        sequence!($pm, pt, {$($rest)*}, $creator)
    }};
    ($pm:expr, $pt:expr, {$parser:expr; $($rest:tt)*}, $creator:expr) => {{
        let (pt, _) = try_parse!($parser($pm, $pt));
        sequence!($pm, pt, {$($rest)*}, $creator)
    }};
    ($pm:expr, $pt:expr, {}, $creator:expr) => {
        Progress::success($pt, $creator($pm, $pt))
    };
}

// TODO: promote?
fn comma_tail<'s, F, T>(f: F) -> impl Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
    where F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
{
    move |pm, pt| {
        sequence!(pm, pt, {
            v  = f;
            _x = optional(whitespace);
            _x = optional(literal(","));
            _x = optional(whitespace);
        }, |_, _| v)
    }
}

// TODO: promote?
fn pipe_tail<'s, F, T>(f: F) -> impl Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
    where F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
{
    move |pm, pt| {
        sequence!(pm, pt, {
            v  = f;
            _x = optional(whitespace);
            _x = optional(literal("|"));
            _x = optional(whitespace);
        }, |_, _| v)
    }
}

// TODO: promote?
fn plus_tail<'s, F, T>(f: F) -> impl Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
    where F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
{
    move |pm, pt| {
        sequence!(pm, pt, {
            v  = f;
            _x = optional(whitespace);
            _x = optional(literal("+"));
            _x = optional(whitespace);
        }, |_, _| v)
    }
}

// TODO: promote?
fn optional<'s, F, T>(f: F) -> impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Option<T>>
    where F: FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
{
    move |pm, pt| pm.optional(pt, f)
}

// TODO: promote?
#[allow(dead_code)]
fn optional_append<'s, A, F, T>(a: A, f: F) -> impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Vec<T>>
    where F: FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, T>,
          A: IntoAppend<T>,
{
    move |pm, pt| {
        let mut a = a.into();
        match f(pm, pt) {
            Progress { point, status: peresil::Status::Success(v) } => {
                a.push(v);
                Progress::success(point, a)
            }
            Progress { point, status: peresil::Status::Failure(..) } => {
                Progress::success(point, a)
            }
        }
    }
}

// TODO: promote?
fn point<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Point<'s>> {
    Progress::success(pt, pt)
}

// TODO: promote?
fn zero_or_more<'s, F, T>(f: F) -> impl Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, Vec<T>>
    where F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
{
    move |pm, pt| pm.zero_or_more(pt, &f) // what why ref?
}

trait IntoAppend<T> {
    fn into(self) -> Vec<T>;
}
impl<T> IntoAppend<T> for Vec<T> {
    fn into(self) -> Vec<T> { self }
}
impl<T> IntoAppend<T> for Option<T> {
    fn into(self) -> Vec<T> {
        self.map(|v| vec![v]).unwrap_or_else(Vec::new)
    }
}

// TODO: promote?
fn zero_or_more_append<'s, A, F, T>(existing: A, f: F) -> impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Vec<T>>
    where F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>,
          A: IntoAppend<T>,
{
    move |pm, mut pt| {
        let mut existing = existing.into();
        loop {
            match f(pm, pt) {
                Progress { point, status: peresil::Status::Success(v) } => {
                    existing.push(v);
                    pt = point;
                },
                Progress { point, .. } => return Progress::success(point, existing),
            }
        }
    }
}

#[allow(dead_code)]
fn map<'s, P, F, T, U>(p: P, f: F) -> impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, U>
    where P: FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, T>,
          F: FnOnce(T) -> U
{
    move |pm, pt| {
        p(pm, pt).map(f)
    }
}

// todo: promote?
#[allow(dead_code)]
fn inspect<'s, F>(f: F) -> impl Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, ()>
    where F: Fn(Point<'s>)
{
    move |_, pt| {
        f(pt);
        Progress::success(pt, ())
    }
}

// TODO: can we transofrm this to (pm, pt)?
fn top_level<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TopLevel> {
    pm.alternate(pt)
        .one(function)
        .one(map(macro_rules, TopLevel::MacroRules))
        .one(map(p_struct, TopLevel::Struct))
        .one(map(p_enum, TopLevel::Enum))
        .one(map(p_trait, TopLevel::Trait))
        .one(map(p_impl, TopLevel::Impl))
        .one(map(attribute, TopLevel::Attribute))
        .one(extern_crate)
        .one(map(p_use, TopLevel::Use))
        .one(map(type_alias, TopLevel::TypeAlias))
        .one(map(module, TopLevel::Module))
        .one(map(whitespace, TopLevel::Whitespace))
        .finish()
}

fn literal<'s>(expected: &'static str) -> impl Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, &'s str> {
    move |_pm, pt| pt.consume_literal(expected).map_err(|_| Error::Literal(expected))
}

fn comment<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Comment> {
    pm.alternate(pt)
        .one(comment_end_of_line)
        .one(comment_region)
        .finish()
}

fn comment_end_of_line<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Comment> {
    let spt = pt;
    sequence!(pm, pt, {
        _x   = literal("//");
        text = parse_until2("\n");
    }, |_, pt| Comment { extent: ex(spt, pt), text })
}

fn comment_region<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Comment> {
    let spt = pt;
    sequence!(pm, pt, {
        _x   = literal("/*");
        text = parse_until2("*/");
        _x   = literal("*/");
    }, |_, pt| Comment { extent: ex(spt, pt), text })
}

fn function<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TopLevel> {
    let spt          = pt;
    sequence!(pm, pt, {
        header = function_header;
        _x     = optional(whitespace);
        body   = block;
    }, |_, pt| TopLevel::Function(Function {
        extent: ex(spt, pt),
        header,
        body,
    }))
}

fn ext<'s, F, T>(f: F) -> impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Extent>
    where F: FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
{
    move |pm, pt| {
        let spt = pt;
        let (pt, _) = try_parse!(f(pm, pt));
        Progress::success(pt, ex(spt, pt))
    }
}

fn optional_whitespace<'s>(ws: Vec<Whitespace>) -> impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Vec<Whitespace>> {
    zero_or_more_append(ws, whitespace_core)
}

fn function_header<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, FunctionHeader> {
    let ws = Vec::new();
    let spt = pt;
    sequence!(pm, pt, {
        visibility  = optional(ext(literal("pub")));
        ws          = optional_whitespace(ws);
        _x          = literal("fn");
        ws          = optional_whitespace(ws);
        name        = ident;
        generics    = optional(function_generic_declarations);
        arguments   = function_arglist;
        ws          = optional_whitespace(ws);
        return_type = optional(function_return_type);
        ws          = optional_whitespace(ws);
        wheres      = optional(function_where_clause);
    }, |_, pt| {
        FunctionHeader {
            extent: ex(spt, pt),
            visibility,
            name,
            generics,
            arguments,
            return_type,
            wheres: wheres.unwrap_or_else(Vec::new),
            whitespace: ws,
        }})
}

fn macro_rules<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, MacroRules> {
    let spt = pt;
    sequence!(pm, pt, {
        _x   = literal("macro_rules!");
        _x   = whitespace;
        name = ident;
        _x   = whitespace;
        _x   = literal("{");
        body = parse_nested_until('{', '}');
        _x   = literal("}");
    }, |_, pt| MacroRules {
        extent: ex(spt, pt),
        name,
        body,
    })
}

fn ident<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    let spt = pt;
    let (pt, ex) = parse_until(pt, |c| {
        [
            '[', ']', '(', ')', '<', '>', '{', '}',
            '!', ' ', ':', ',', ';', '/', '.', '=',
            '|', '\'', '"', '#',
        ].contains(&c)
    });
    if pt.offset <= spt.offset {
        Progress::failure(pt, Error::IdentNotFound)
    } else {
        Progress::success(pt, ex)
    }
}

fn function_generic_declarations<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, GenericDeclarations> {
    sequence!(pm, pt, {
        _x        = literal("<");
        lifetimes = zero_or_more(comma_tail(lifetime));
        types     = zero_or_more(comma_tail(generic_declaration));
        _x        = literal(">");
    }, |_, _| GenericDeclarations { lifetimes, types })
}

fn generic_declaration<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Generic> {
    ident(pm, pt)
}

fn function_arglist<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Vec<Argument>> {
    sequence!(pm, pt, {
        _x       = literal("(");
        self_arg = optional(self_argument);
        args     = zero_or_more_append(self_arg, comma_tail(function_argument));
        _x       = literal(")");
    }, move |_, _| args)
}

fn self_argument<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Argument> {
    sequence!(pm, pt, {
        _x = optional(literal("&"));
        _x = optional(whitespace);
        _x = optional(literal("mut"));
        _x = optional(whitespace);
        _x = literal("self");
        _x = optional(literal(","));
    }, |_, _| Argument::SelfArgument)
}

fn function_argument<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Argument> {
    sequence!(pm, pt, {
        name = ident;
        _x   = literal(":");
        _x   = optional(whitespace);
        typ  = typ;
    }, |_, _| Argument::Named { name, typ })
}

fn function_return_type<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Type> {
    sequence!(pm, pt, {
        _x  = literal("->");
        _x  = optional(whitespace);
        typ = typ;
    }, |_, _| typ)
}

fn function_where_clause<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Vec<Where>> {
    sequence!(pm, pt, {
        _x = literal("where");
        _x = whitespace;
        w  = one_or_more(comma_tail(function_where));
    }, |_, _| w)
}

fn function_where<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Where> {
    let spt = pt;
    sequence!(pm, pt, {
        name   = ident;
        _x     = literal(":");
        _x     = optional(whitespace);
        bounds = one_or_more(plus_tail(typ));
    }, |_, pt| Where { extent: ex(spt, pt), name, bounds })
}

fn block<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Block> {
    let spt = pt;
    sequence!(pm, pt, {
        _x        = literal("{");
        _x        = optional(whitespace);
        mut stmts = zero_or_more(statement);
        mut expr  = optional(expression);
        _x        = optional(whitespace);
        _x        = literal("}");
    }, |_, pt| {
        if expr.is_none() && stmts.last().map_or(false, Statement::is_implicit) {
            expr = stmts.pop().and_then(Statement::implicit);
        }

        Block {
            extent: ex(spt, pt),
            statements: stmts,
            expression: expr,
        }
    })
}

fn statement<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Statement> {
    sequence!(pm, pt, {
        _x   = optional(whitespace);
        expr = statement_inner;
        _x   = optional(whitespace);
    }, |_, _| expr)
}

fn statement_inner<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Statement> {
    pm.alternate(pt)
        .one(explicit_statement)
        .one(implicit_statement)
        .one(map(p_use, Statement::Use))
        .finish()
}

fn explicit_statement<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Statement> {
    sequence!(pm, pt, {
        expr = expression;
        _x = literal(";");
    }, |_, _| Statement::Explicit(expr))
}

// idea: trait w/associated types to avoid redefin fn types?

fn implicit_statement<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Statement> {
    let spt = pt;
    let (pt, kind) = try_parse!(expression_ending_in_brace(pm, pt));

    Progress::success(pt, Statement::Implicit(Expression { extent: ex(spt, pt), kind: kind }))
}

fn expression_ending_in_brace<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ExpressionKind> {
    pm.alternate(pt)
        .one(map(expr_if, ExpressionKind::If))
        .one(map(expr_for_loop, ExpressionKind::ForLoop))
        .one(map(expr_loop, ExpressionKind::Loop))
        .one(map(expr_match, ExpressionKind::Match))
        .one(map(expr_block, ExpressionKind::Block))
        .finish()
}

fn expression<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Expression> {
    let spt        = pt;
    let (pt, _)    = try_parse!(optional(whitespace)(pm, pt));
    let (pt, kind) = try_parse!({
        pm.alternate(pt)
            .one(expression_ending_in_brace)
            .one(map(expr_macro_call, ExpressionKind::MacroCall))
            .one(map(expr_let, ExpressionKind::Let))
            .one(map(expr_assign, ExpressionKind::Assign))
            .one(map(expr_function_call, ExpressionKind::FunctionCall))
            .one(map(expr_tuple, ExpressionKind::Tuple))
            .one(map(expr_range, ExpressionKind::Range))
            .one(map(expr_array, ExpressionKind::Array))
            .one(map(character_literal, ExpressionKind::Character))
            .one(map(string_literal, ExpressionKind::String))
            .one(map(expr_closure, ExpressionKind::Closure))
            .one(map(expr_return, ExpressionKind::Return))
            .one(map(expr_value, ExpressionKind::Value))
            .finish()
    });
    let mpt = pt;

    let mut expression = Expression {
        extent: ex(spt, mpt),
        kind,
    };

    let mut pt = pt;
    loop {
        let (pt2, tail) = try_parse!(optional(expression_tail)(pm, pt));
        pt = pt2;
        match tail {
            Some(ExpressionTail::Binary { op, rhs }) => {
                expression = Expression {
                    extent: ex(spt, pt),
                    kind: ExpressionKind::Binary(Binary {
                        op: op,
                        lhs: Box::new(expression),
                        rhs: rhs,
                    })
                }
            }
            Some(ExpressionTail::FieldAccess { field }) => {
                expression = Expression {
                    extent: ex(spt, pt),
                    kind: ExpressionKind::FieldAccess(FieldAccess {
                        value: Box::new(expression),
                        field: field,
                    })
                }
            }
            Some(ExpressionTail::Call { args }) => {
                expression = Expression {
                    extent: ex(spt, pt),
                    kind: ExpressionKind::Call(Call {
                        target: Box::new(expression),
                        args: args
                    })
                }
            }
            Some(ExpressionTail::MethodCall { name, turbofish, args }) => {
                expression = Expression {
                    extent: ex(spt, pt),
                    kind: ExpressionKind::MethodCall(MethodCall {
                        receiver: Box::new(expression),
                        name: name,
                        turbofish: turbofish,
                        args: args
                    })
                }
            }
            Some(ExpressionTail::Range { rhs }) => {
                expression = Expression {
                    extent: ex(spt, pt),
                    kind: ExpressionKind::Range(Range {
                        lhs: Some(Box::new(expression)),
                        rhs
                    })
                }

            }
            Some(ExpressionTail::Slice { range }) => {
                expression = Expression {
                    extent: ex(spt, pt),
                    kind: ExpressionKind::Slice(Slice {
                        target: Box::new(expression),
                        range
                    })
                }

            }
            None => break,
        }
    }

    Progress::success(pt, expression)
}

fn expr_macro_call<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, MacroCall> {
    sequence!(pm, pt, {
        name = ident;
        _x   = literal("!");
        args = expr_macro_call_args;
    }, |_, _| MacroCall { name, args })
}

fn expr_macro_call_args<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    pm.alternate(pt)
        .one(expr_macro_call_paren)
        .one(expr_macro_call_square)
        .finish()
}

fn expr_macro_call_paren<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    sequence!(pm, pt, {
        _x   = literal("(");
        args = parse_nested_until('(', ')');
        _x   = literal(")");
    }, |_, _| args)
}

fn expr_macro_call_square<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    sequence!(pm, pt, {
        _x   = literal("[");
        args = parse_nested_until('[', ']');
        _x   = literal("]");
    }, |_, _| args)
}

fn expr_let<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Let> {
    sequence!(pm, pt, {
        _x      = literal("let");
        _x      = whitespace;
        pattern = pattern;
        _x      = optional(whitespace);
        typ     = optional(expr_let_type);
        _x      = optional(whitespace);
        value   = optional(expr_let_rhs);
    }, |_, _| Let { pattern, typ, value: value.map(Box::new) })
}

fn expr_let_type<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Type> {
    sequence!(pm, pt, {
        _x  = literal(":");
        _x  = optional(whitespace);
        typ = typ;
    }, |_, _| typ)
}

fn expr_let_rhs<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Expression> {
    sequence!(pm, pt, {
        _x    = literal("=");
        _x    = optional(whitespace);
        value = expression;
    }, |_, _| value)
}

fn expr_assign<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Assign> {
    sequence!(pm, pt, {
        name  = ident;
        _x    = optional(whitespace);
        _x    = literal("=");
        _x    = optional(whitespace);
        value = expression;
    }, |_, _| Assign { name, value: Box::new(value) })
}

fn expr_if<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, If> {
    let spt = pt;
    sequence!(pm, pt, {
        _x                = literal("if");
        _x                = whitespace;
        (condition, body) = expr_followed_by_block;
        more              = zero_or_more(expr_if_else_if);
        else_body         = optional(expr_if_else_end);
    }, move |_, pt| If {
        extent: ex(spt, pt),
        condition: Box::new(condition),
        body: Box::new(body),
        more,
        else_body: else_body.map(Box::new)
    })
}

fn expr_if_else_if<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, If> {
    sequence!(pm, pt, {
        _x   = optional(whitespace);
        _x   = literal("else");
        _x   = optional(whitespace);
        tail = expr_if;
    }, |_, _| tail)
}

fn expr_if_else_end<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Block> {
    sequence!(pm, pt, {
        _x   = optional(whitespace);
        _x   = literal("else");
        _x        = optional(whitespace);
        else_body = block;
    }, |_, _| else_body)
}

// `expr {}` greedily matches `StructName {}` as a structure literal
// and then fails because the body isn't found. Since this could be
// valid if `StructName` were a local variable, we force backtracking
// if we didn't find the body. This means we duplicate some grammar
// from `expression`.
fn expr_followed_by_block<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, (Expression, Block)> {
    pm.alternate(pt)
        .one(expr_followed_by_block_expr)
        .one(expr_followed_by_block_simple)
        .finish()
}

fn expr_followed_by_block_expr<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, (Expression, Block)> {
    sequence!(pm, pt, {
        condition = expression;
        _x        = optional(whitespace);
        body      = block;
    }, |_, _| (condition, body))
}

fn expr_followed_by_block_simple<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, (Expression, Block)> {
    let spt = pt;
    sequence!(pm, pt, {
        condition = pathed_ident;
        mpt       = point;
        _x        = optional(whitespace);
        body      = block;
    }, |_, _| {
        let condition = Expression {
            extent: ex(spt, mpt),
            kind: ExpressionKind::Value(Value { name: condition, literal: None }),
        };
        (condition, body)
    })
}

fn expr_for_loop<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ForLoop> {
    sequence!(pm, pt, {
        _x           = literal("for");
        _x           = whitespace;
        pattern      = pattern;
        _x           = whitespace;
        _x           = literal("in");
        _x           = whitespace;
        (iter, body) = expr_followed_by_block;
    }, |_, _| ForLoop { pattern, iter: Box::new(iter), body: Box::new(body) })
}

fn expr_loop<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Loop> {
    sequence!(pm, pt, {
        _x   = literal("loop");
        _x   = optional(whitespace);
        body = block;
    }, |_, _| Loop { body: Box::new(body) })
}

fn expr_match<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Match> {
    sequence!(pm, pt, {
        _x   = literal("match");
        _x   = whitespace;
        head = expression;
        _x   = optional(whitespace);
        _x   = literal("{");
        arms = zero_or_more(match_arm);
        _x   = literal("}");
    }, |_, _| Match { head: Box::new(head), arms })
}

fn match_arm<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, MatchArm> {
    let spt = pt;
    sequence!(pm, pt, {
        _x      = optional(whitespace);
        pattern = one_or_more(pipe_tail(pattern));
        _x      = optional(whitespace);
        _x      = literal("=>");
        _x      = optional(whitespace);
        body    = expression;
        _x      = optional(whitespace);
        _x      = optional(literal(","));
        _x      = optional(whitespace);
    }, |_, pt| MatchArm { extent: ex(spt, pt), pattern, body })
}

fn expr_tuple<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Tuple> {
    sequence!(pm, pt, {
        _x      = literal("(");
        members = zero_or_more(comma_tail(expression));
        _x      = literal(")");
    }, |_, _| Tuple { members })
}

fn expr_range<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Range> {
    sequence!(pm, pt, {
        _x  = literal("..");
        rhs = optional(expression);
    }, |_, _| Range { lhs: None, rhs: rhs.map(Box::new) } )
}

fn expr_array<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Array> {
    sequence!(pm, pt, {
        _x      = literal("[");
        members = zero_or_more(comma_tail(expression));
        _x      = literal("]");
    }, |_, _| Array { members })
}

fn character_literal<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Character> {
    sequence!(pm, pt, {
        _x    = literal("'");
        value = ext(char_char);
        _x    = literal("'");
    }, |_, _| Character { value })
}

fn char_char<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, &'s str> {
    let res = |i| {
        let (head, tail) = pt.s.split_at(i);
        let pt = Point { s: tail, offset: pt.offset + i };
        Progress::success(pt, head)
    };

    let mut escaped = false;
    for (i, c) in pt.s.char_indices() {
        match (escaped, c) {
            (true, _) => escaped = false,
            (false, '\\') => escaped = true,
            (false, '\'') => return res(i),
            (false, _) => { /* Next char */ },
        }
    }

    res(pt.s.len())
}

fn string_literal<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, String> {
    pm.alternate(pt)
        .one(string_literal_normal)
        .one(string_literal_raw)
        .finish()
}

fn string_literal_normal<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, String> {
    sequence!(pm, pt, {
        _x    = literal("\"");
        value = ext(str_char);
        _x    = literal("\"");
    }, |_, _| String { value })
}

fn str_char<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, &'s str> {
    let res = |i| {
        let (head, tail) = pt.s.split_at(i);
        let pt = Point { s: tail, offset: pt.offset + i };
        Progress::success(pt, head)
    };

    let mut escaped = false;
    for (i, c) in pt.s.char_indices() {
        match (escaped, c) {
            (true, _) => escaped = false,
            (false, '\\') => escaped = true,
            (false, '"') => return res(i),
            (false, _) => { /* Next char */ },
        }
    }

    res(pt.s.len())
}

fn string_literal_raw<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, String> {
    sequence!(pm, pt, {
        _x    = literal("r");
        h = zero_or_more(literal("#"));
        _x = literal(r#"""#);
        value = ext(raw_raw(h.len()));
    }, |_, _| String { value })
}

fn raw_raw<'s>(hashes: usize) -> impl Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, &'s str> {
    let mut s = r#"""#.to_string();
    for _ in 0..hashes { s.push('#') };

    move |_, pt| {
        match pt.s.find(&s) {
            Some(end) => {
                let (str_content, quote_tail) = pt.s.split_at(end);
                let (_quotes, tail) = quote_tail.split_at(s.len());
                let pt = Point { s: tail, offset: pt.offset + end + s.len() };
                Progress::success(pt, str_content)
            }
            None => {
                Progress::failure(pt, Error::UnterminatedRawString)
            }
        }
    }
}

fn expr_closure<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Closure> {
    sequence!(pm, pt, {
        mov  = optional(literal("move"));
        _x   = optional(whitespace);
        _x   = literal("|");
        args = zero_or_more(comma_tail(expr_closure_arg));
        _x   = literal("|");
        body = expression;
    }, |_, _| Closure { is_move: mov.is_some(), args, body: Box::new(body) })
}

fn expr_closure_arg<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ClosureArg> {
    sequence!(pm, pt, {
        name = ident;
        typ  = optional(expr_closure_arg_type);
    }, |_, _| ClosureArg { name, typ })
}

fn expr_closure_arg_type<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Type> {
    sequence!(pm, pt, {
        _x  = optional(whitespace);
        _x  = literal(":");
        _x  = optional(whitespace);
        typ = typ;
    }, |_, _| typ)
}

fn expr_return<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Return> {
    let spt = pt;
    sequence!(pm, pt, {
        _x    = optional(literal("return"));
        _x    = whitespace;
        value = expression;
    }, |_, pt| Return { extent: ex(spt, pt), value: Box::new(value) })
}

fn expr_block<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Box<Block>> {
    block(pm, pt).map(Box::new)
}

fn expr_value<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Value> {
    sequence!(pm, pt, {
        name    = pathed_ident;
        _x      = optional(whitespace);
        literal = optional(expr_value_struct_literal);
    }, |_, _| Value { name, literal } )
}

fn expr_value_struct_literal<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Vec<StructLiteralField>> {
    sequence!(pm, pt, {
        _x     = literal("{");
        _x     = optional(whitespace);
        fields = zero_or_more(comma_tail(expr_value_struct_literal_field));
        _x     = optional(whitespace);
        _x     = literal("}");
    }, |_, _| fields)
}

fn expr_value_struct_literal_field<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, StructLiteralField> {
    let spt = pt;
    sequence!(pm, pt, {
        name  = ident;
        mpt   = point;
        _x    = optional(whitespace);
        value = optional(expr_value_struct_literal_field_value);
    }, |_, _| {
        let value = value.unwrap_or_else(|| Expression {
            extent: ex(spt, mpt),
            kind: ExpressionKind::Value(Value { name, literal: None }),
        });
        StructLiteralField { name, value }
    })
}

fn expr_value_struct_literal_field_value<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Expression> {
    sequence!(pm, pt, {
        _x    = literal(":");
        _x    = optional(whitespace);
        value = expression;
    }, |_, _| value )
}

fn expr_function_call<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, FunctionCall> {
    sequence!(pm, pt, {
        name = pathed_ident;
        _x   = literal("(");
        args = zero_or_more(comma_tail(expression));
        _x   = literal(")");
    }, |_, _| FunctionCall { name, args })
}

fn expression_tail<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ExpressionTail> {
    pm.alternate(pt)
        .one(expr_tail_binary)
        .one(expr_tail_call)
        .one(expr_tail_method_call)
        .one(expr_tail_field_access)
        .one(expr_tail_range)
        .one(expr_tail_slice)
        .finish()
}

fn expr_tail_binary<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ExpressionTail> {
    sequence!(pm, pt, {
        _x  = optional(whitespace);
        op  = binary_op;
        _x  = optional(whitespace);
        rhs = expression;
    }, |_, _| ExpressionTail::Binary { op, rhs: Box::new(rhs) })
}

fn binary_op<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    // Two characters before one to avoid matching += as +
    pm.alternate(pt)
        .one(ext(literal("!=")))
        .one(ext(literal("==")))
        .one(ext(literal("&&")))
        .one(ext(literal("||")))
        .one(ext(literal("+=")))
        .one(ext(literal("-=")))
        .one(ext(literal("*=")))
        .one(ext(literal("/=")))
        .one(ext(literal("%=")))
        .one(ext(literal("<=")))
        .one(ext(literal(">=")))
        .one(ext(literal("+")))
        .one(ext(literal("-")))
        .one(ext(literal("*")))
        .one(ext(literal("/")))
        .one(ext(literal("%")))
        .one(ext(literal("<")))
        .one(ext(literal(">")))
        .finish()
}

fn expr_tail_method_call<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ExpressionTail> {
    sequence!(pm, pt, {
        _x        = optional(whitespace);
        _x        = literal(".");
        name      = ident;
        turbofish = optional(turbofish);
        _x        = literal("(");
        args      = zero_or_more(comma_tail(expression));
        _x        = literal(")");
    }, |_, _| ExpressionTail::MethodCall { name, turbofish, args })
}

fn expr_tail_call<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ExpressionTail> {
    sequence!(pm, pt, {
        _x        = literal("(");
        args      = zero_or_more(comma_tail(expression));
        _x        = literal(")");
    }, |_, _| ExpressionTail::Call { args })
}

fn expr_tail_field_access<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ExpressionTail> {
    sequence!(pm, pt, {
        _x = literal(".");
        field = ident;
    }, |_, _| ExpressionTail::FieldAccess { field })
}

fn expr_tail_range<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ExpressionTail> {
    sequence!(pm, pt, {
        _x  = literal("..");
        rhs = optional(expression);
    }, |_, _| ExpressionTail::Range { rhs: rhs.map(Box::new) })
}

fn expr_tail_slice<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ExpressionTail> {
    sequence!(pm, pt, {
        _x    = literal("[");
        _x    = optional(whitespace);
        range = expression;
        _x    = optional(whitespace);
        _x    = literal("]");
    }, |_, _| ExpressionTail::Slice { range: Box::new(range) })
}

fn pathed_ident<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    let spt = pt;
    sequence!(pm, pt, {
        _x = ident;
        _x = zero_or_more(path_component);
        _x = optional(turbofish);
    }, |_, pt| ex(spt, pt))
}

fn path_component<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    let spt = pt;
    sequence!(pm, pt, {
        _x = literal("::");
        _x = ident;
    }, |_, pt| ex(spt, pt))
}

fn turbofish<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    sequence!(pm, pt, {
        _x    = literal("::<");
        types = ext(one_or_more(comma_tail(typ)));
        _x    = literal(">");
    }, |_, _| types)
}

fn pattern<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Pattern> {
    pm.alternate(pt)
        .one(pattern_struct)
        .one(pattern_ident)
        .one(pattern_tuple)
        .one(pattern_char)
        .finish()
}

fn pattern_ident<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Pattern> {
    let spt = pt;
    sequence!(pm, pt, {
        _x    = optional(literal("mut"));
        _x    = optional(whitespace);
        ident = pathed_ident;
        tuple = optional(pattern_tuple_inner);
    }, |_, pt| Pattern::Ident { extent: ex(spt, pt), ident, tuple: tuple.unwrap_or_else(Vec::new) })
}

fn pattern_tuple<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Pattern> {
    let spt = pt;
    let (pt, members) = try_parse!(pattern_tuple_inner(pm, pt));
    Progress::success(pt, Pattern::Tuple { extent: ex(spt, pt), members })
}

fn pattern_tuple_inner<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Vec<Pattern>> {
    sequence!(pm, pt, {
        _x               = literal("(");
        mut sub_patterns = zero_or_more(comma_tail(pattern));
        wildcard         = optional(ext(literal("..")));
        _x               = literal(")");
    }, |_, _| {
        if let Some(extent) = wildcard {
            sub_patterns.push(Pattern::Wildcard { extent });
        }
        sub_patterns
    })
}

fn pattern_struct<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Pattern> {
    let spt = pt;
    sequence!(pm, pt, {
        name     = pathed_ident;
        _x       = optional(whitespace);
        _x       = literal("{");
        _x       = optional(whitespace);
        fields   = zero_or_more(comma_tail(pattern_struct_field));
        _x       = optional(whitespace);
        wildcard = optional(literal(".."));
        _x       = optional(whitespace);
        _x       = literal("}");
    }, |_, pt| Pattern::Struct {
        extent: ex(spt, pt),
        name,
        fields,
        wildcard: wildcard.is_some(),
    })
}

fn pattern_struct_field<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternStructField> {
    sequence!(pm, pt, {
        name    = ident;
        _x      = optional(whitespace);
        pattern = optional(pattern_struct_field_tail);
    }, |_, _| {
        let pattern = pattern.unwrap_or_else(|| {
            Pattern::Ident { extent: name, ident: name, tuple: Vec::new() }
        });
        PatternStructField { name, pattern }
    })
}

fn pattern_struct_field_tail<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Pattern> {
    sequence!(pm, pt, {
        _x      = literal(":");
        _x      = optional(whitespace);
        pattern = pattern;
    }, |_, _| pattern)
}

fn pattern_char<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Pattern> {
    let spt = pt;
    let (pt, value) = try_parse!(character_literal(pm, pt));
    Progress::success(pt, Pattern::Character { extent: ex(spt, pt), value })
}

fn p_struct<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Struct> {
    let spt = pt;
    sequence!(pm, pt, {
        _x     = optional(literal("pub"));
        _x     = optional(whitespace);
        _x     = literal("struct");
        _x     = whitespace;
        name   = ident;
        _x     = optional(whitespace);
        fields = struct_defn_body;
    }, |_, pt| Struct {
        extent: ex(spt, pt),
        name,
        fields,
    })
}

fn struct_defn_body<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Vec<StructField>> {
    sequence!(pm, pt, {
        _x = literal("{");
        _x = optional(whitespace);
        fields = zero_or_more(comma_tail(struct_defn_field));
        _x = optional(whitespace);
        _x = literal("}");
    }, |_, _| fields)
}

fn struct_defn_field<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, StructField> {
    let spt = pt;
    sequence!(pm, pt, {
        name = ident;
        _x   = literal(":");
        _x   = optional(whitespace);
        typ  = typ;
    }, |_, pt| StructField { extent: ex(spt, pt), name, typ })
}

fn p_enum<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Enum> {
    let spt = pt;
    sequence!(pm, pt, {
        _x       = optional(literal("pub"));
        _x       = optional(whitespace);
        _x       = literal("enum");
        _x       = whitespace;
        name     = ident;
        _x       = optional(whitespace);
        _x       = literal("{");
        _x       = optional(whitespace);
        variants = zero_or_more(comma_tail(enum_variant));
        _x       = optional(whitespace);
        _x       = literal("}");
    }, |_, pt| Enum {
        extent: ex(spt, pt),
        name,
        variants,
    })
}

fn enum_variant<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, EnumVariant> {
    let spt        = pt;
    sequence!(pm, pt, {
        name = ident;
        _x   = optional(whitespace);
        body = optional(enum_variant_body);
    }, |_, pt| EnumVariant {
        extent: ex(spt, pt),
        name,
        body: body
    })
}

fn enum_variant_body<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, EnumVariantBody> {
    pm.alternate(pt)
        .one(map(tuple_defn_body, EnumVariantBody::Tuple))
        .one(map(ext(struct_defn_body), EnumVariantBody::Struct))
        .finish()
}

fn p_trait<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Trait> {
    let spt        = pt;
    sequence!(pm, pt, {
        _x       = literal("trait");
        _x       = whitespace;
        name     = ident;
        generics = optional(function_generic_declarations);
        _x       = whitespace;
        _x       = literal("{");
        _x       = optional(whitespace);
        members  = zero_or_more(trait_impl_member);
        _x       = optional(whitespace);
        _x       = literal("}");
    }, |_, pt| Trait { extent: ex(spt, pt), name, generics, members })
}

fn trait_impl_member<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TraitMember> {
    pm.alternate(pt)
        .one(map(trait_impl_function, TraitMember::Function))
        .one(map(attribute, TraitMember::Attribute))
        .one(map(whitespace, TraitMember::Whitespace))
        .finish()
}

fn trait_impl_function<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TraitImplFunction> {
    let spt = pt;
    sequence!(pm, pt, {
        header = function_header;
        body   = trait_impl_function_body;
    }, |_, pt| TraitImplFunction { extent: ex(spt, pt), header, body })
}

fn trait_impl_function_body<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Option<Block>> {
    pm.alternate(pt)
        .one(map(block, Some))
        .one(map(literal(";"), |_| None))
        .finish()
}

fn p_impl<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Impl> {
    let spt = pt;
    sequence!(pm, pt, {
        _x         = literal("impl");
        generics   = optional(function_generic_declarations);
        _x         = whitespace;
        trait_name = optional(p_impl_of_trait);
        type_name  = typ;
        _x         = optional(whitespace);
        _x         = literal("{");
        _x         = optional(whitespace);
        body       = zero_or_more(impl_member);
        _x         = optional(whitespace);
        _x         = literal("}");
    }, |_, pt| Impl {
        extent: ex(spt, pt),
        generics,
        trait_name,
        type_name,
        body,
    })
}

fn p_impl_of_trait<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Type> {
    sequence!(pm, pt, {
        trait_name = typ;
        _x         = whitespace;
        _x         = literal("for");
        _x         = whitespace;
    }, |_, _| trait_name)
}

fn impl_member<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ImplMember> {
    pm.alternate(pt)
        .one(map(impl_function, ImplMember::Function))
        .one(map(attribute, ImplMember::Attribute))
        .one(map(whitespace, ImplMember::Whitespace))
        .finish()
}

fn impl_function<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ImplFunction> {
    let spt = pt;
    sequence!(pm, pt, {
        header = function_header;
        body   = block;
    }, |_, pt| ImplFunction { extent: ex(spt, pt), header, body })
}

// TODO: optional could take E that is `into`, or just a different one

fn attribute<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Attribute> {
    let spt = pt;
    sequence!(pm, pt, {
        _x = literal("#");
        _x = optional(literal("!"));
        _x = literal("[");
        _x = parse_nested_until('[', ']');
        _x = literal("]");
    }, |_, pt| ex(spt, pt))
}

fn extern_crate<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TopLevel> {
    let spt = pt;
    sequence!(pm, pt, {
        _x   = literal("extern");
        _x   = whitespace;
        _x   = literal("crate");
        _x   = whitespace;
        name = ident;
        _x   = optional(whitespace);
        _x   = literal(";");
    }, |_, pt| TopLevel::ExternCrate(Crate {
        extent: ex(spt, pt),
        name,
    }))
}

fn p_use<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Use> {
    let spt = pt;
    sequence!(pm, pt, {
        _x   = literal("use");
        _x   = whitespace;
        name = use_path;
        _x   = literal(";");
    }, |_, pt| Use {
        extent: ex(spt, pt),
        name
    })
}

fn use_path<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    let spt = pt;
    sequence!(pm, pt, {
        _x = ident;
        _x = zero_or_more(use_path_component);
        _x = optional(use_path_tail);
    }, |_, pt| ex(spt, pt))
}

fn use_path_component<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    let spt = pt;
    sequence!(pm, pt, {
        _x = literal("::");
        _x = ident;
    }, |_, pt| ex(spt, pt))
}

fn use_path_tail<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    let spt = pt;
    sequence!(pm, pt, {
        _x = literal("::*");
    }, |_, pt| ex(spt, pt))
}

fn type_alias<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TypeAlias> {
    let spt = pt;
    sequence!(pm, pt, {
        _x   = literal("type");
        _x   = whitespace;
        name = typ;
        _x   = optional(whitespace);
        _x   = literal("=");
        _x   = optional(whitespace);
        defn = typ;
        _x   = optional(whitespace);
        _x   = literal(";");
    }, |_, pt| TypeAlias {
        extent: ex(spt, pt),
        name,
        defn,
    })
}

fn module<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Module> {
    let spt = pt;
    sequence!(pm, pt, {
        _x   = literal("mod");
        _x   = whitespace;
        name = ident;
        _x   = optional(whitespace);
        _x   = literal("{");
        body = zero_or_more(top_level);
        _x   = literal("}");
    }, |_, pt| Module { extent: ex(spt, pt), name, body })
}

fn typ<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    let spt = pt;
    sequence!(pm, pt, {
        _x = optional(typ_ref);
        _x = optional(whitespace);
        _x = typ_inner;
    }, |_, pt| ex(spt, pt))
}

fn typ_ref<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    let spt = pt;
    sequence!(pm, pt, {
        _x = literal("&");
        _x = optional(whitespace);
        _x = optional(literal("mut"));
        _x = optional(whitespace);
        _x = optional(lifetime);
    }, |_, pt| ex(spt, pt))
}

fn typ_inner<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    pm.alternate(pt)
        .one(typ_core)
        .one(tuple_defn_body)
        .finish()
}

fn tuple_defn_body<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    let spt = pt;
    sequence!(pm, pt, {
        _x = literal("(");
        _x = zero_or_more(comma_tail(typ));
        _x = literal(")");
    }, |_, pt| ex(spt, pt))
}

fn typ_core<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    let spt = pt;
    sequence!(pm, pt, {
        _x = optional(typ_impl);
        _x = pathed_ident;
        _x = optional(typ_generics);
    }, |_, pt| ex(spt, pt))
}

fn typ_impl<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ()> {
    sequence!(pm, pt, {
        _x = literal("impl");
        _x = whitespace;
    }, |_, _| ())
}

fn typ_generics<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    pm.alternate(pt)
        .one(typ_generics_fn)
        .one(typ_generics_angle)
        .finish()
}

fn typ_generics_fn<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    let spt = pt;
    sequence!(pm, pt, {
        _x = literal("(");
        _x = optional(one_or_more(comma_tail(typ)));
        _x = literal(")");
        _x = optional(whitespace);
        _x = optional(function_return_type);
    }, |_, pt| ex(spt, pt))
}

fn typ_generics_angle<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    let spt = pt;
    sequence!(pm, pt, {
        _x = literal("<");
        _x = optional(whitespace);
        _x = optional(one_or_more(comma_tail(lifetime)));
        _x = optional(one_or_more(comma_tail(typ)));
        _x = literal(">");
    }, |_, pt| ex(spt, pt))
}

fn lifetime<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    sequence!(pm, pt, {
        _x   = literal("'");
        _x   = optional(whitespace);
        name = ident;
    }, |_, _| name)
}

fn whitespace<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Vec<Whitespace>> {
    one_or_more(whitespace_core)(pm, pt)
}

fn whitespace_core<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Whitespace> {
    pm.alternate(pt)
        .one(map(comment, Whitespace::Comment))
        .one(map(true_whitespace, Whitespace::Whitespace))
        .finish()
}

fn true_whitespace<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    let spt = pt;

    let (pt, _) = try_parse!(one_or_more(|pm, pt| {
        pm.alternate(pt)
            .one(literal(" "))
            .one(literal("\t"))
            .one(literal("\r"))
            .one(literal("\n"))
            .finish()
    })(pm, pt));

    Progress::success(pt, ex(spt, pt))
}

#[cfg(test)]
mod test {
    use super::*;

    fn qp<'s, F, T>(f: F, s: &'s str) -> peresil::Progress<Point<'s>, T, Vec<Error>>
        where F: FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
    {
        // TODO: Master::once()?
        let mut pm = Master::new();
        let pt = Point::new(s);
        let r = f(&mut pm, pt);
        pm.finish(r)
    }

    #[test]
    fn top_level_use() {
        let p = qp(p_use, "use foo::Bar;");
        assert_eq!(unwrap_progress(p).extent, (0, 13))
    }

    #[test]
    fn top_level_use_wildcard() {
        let p = qp(p_use, "use foo::*;");
        assert_eq!(unwrap_progress(p).extent, (0, 11))
    }

    #[test]
    fn top_level_mod_multiple() {
        let p = qp(top_level, "mod foo { use super::*; }");
        assert_eq!(unwrap_progress(p).extent(), (0, 25))
    }

    #[test]
    fn top_level_macro_rules() {
        let p = qp(macro_rules, "macro_rules! foo { }");
        assert_eq!(unwrap_progress(p).extent, (0, 20))
    }

    #[test]
    fn top_level_mod() {
        let p = qp(module, "mod foo { }");
        assert_eq!(unwrap_progress(p).extent, (0, 11))
    }

    #[test]
    fn top_level_trait() {
        let p = qp(top_level, "trait Foo {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 12))
    }

    #[test]
    fn top_level_trait_with_generics() {
        let p = qp(top_level, "trait Foo<T> {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 15))
    }

    #[test]
    fn top_level_trait_with_members() {
        let p = qp(top_level, "trait Foo { fn bar(&self) -> u8; }");
        assert_eq!(unwrap_progress(p).extent(), (0, 34))
    }

    #[test]
    fn top_level_trait_with_members_with_body() {
        let p = qp(top_level, "trait Foo { fn bar(&self) -> u8 { 42 } }");
        assert_eq!(unwrap_progress(p).extent(), (0, 40))
    }

    #[test]
    fn impl_without_trait() {
        let p = qp(p_impl, "impl Bar {}");
        assert_eq!(unwrap_progress(p).extent, (0, 11))
    }

    #[test]
    fn impl_with_trait() {
        let p = qp(p_impl, "impl Foo for Bar {}");
        assert_eq!(unwrap_progress(p).extent, (0, 19))
    }

    #[test]
    fn impl_with_generics() {
        let p = qp(p_impl, "impl<'a, T> Foo<'a, T> for Bar<'a, T> {}");
        assert_eq!(unwrap_progress(p).extent, (0, 40))
    }

    #[test]
    fn impl_with_attribute() {
        let p = qp(p_impl, "impl Foo { #[attribute] fn bar() {} }");
        assert_eq!(unwrap_progress(p).extent, (0, 37))
    }

    #[test]
    fn impl_with_attributes() {
        let p = qp(p_impl, "impl Foo { #[a] #[b] fn bar() {} }");
        assert_eq!(unwrap_progress(p).extent, (0, 34))
    }

    #[test]
    fn enum_with_trailing_stuff() {
        let p = qp(p_enum, "enum A {} impl Foo for Bar {}");
        assert_eq!(unwrap_progress(p).extent, (0, 9))
    }

    #[test]
    fn enum_with_generic_types() {
        let p = qp(p_enum, "enum A { Foo(Vec<u8>) }");
        assert_eq!(unwrap_progress(p).extent, (0, 23))
    }

    #[test]
    fn enum_with_struct_variant() {
        let p = qp(p_enum, "enum A { Foo { a: u8 } }");
        assert_eq!(unwrap_progress(p).extent, (0, 24))
    }

    #[test]
    fn enum_public() {
        let p = qp(p_enum, "pub enum A {}");
        assert_eq!(unwrap_progress(p).extent, (0, 13))
    }

    #[test]
    fn fn_with_public_modifier() {
        let p = qp(function_header, "pub fn foo()");
        assert_eq!(unwrap_progress(p).extent, (0, 12))
    }

    #[test]
    fn fn_with_self_type() {
        let p = qp(function_header, "fn foo(&self)");
        assert_eq!(unwrap_progress(p).extent, (0, 13))
    }

    #[test]
    fn fn_with_argument() {
        let p = qp(function_header, "fn foo(a: u8)");
        assert_eq!(unwrap_progress(p).extent, (0, 13))
    }

    #[test]
    fn fn_with_argument_with_generic() {
        let p = qp(function_header, "fn foo(a: Vec<u8>)");
        assert_eq!(unwrap_progress(p).extent, (0, 18))
    }

    #[test]
    fn fn_with_arguments() {
        let p = qp(function_header, "fn foo(a: u8, b: u8)");
        assert_eq!(unwrap_progress(p).extent, (0, 20))
    }

    #[test]
    fn fn_with_return_type() {
        let p = qp(function_header, "fn foo() -> bool");
        assert_eq!(unwrap_progress(p).extent, (0, 16))
    }

    #[test]
    fn fn_with_generics() {
        let p = qp(function_header, "fn foo<A, B>()");
        assert_eq!(unwrap_progress(p).extent, (0, 14))
    }

    #[test]
    fn fn_with_lifetimes() {
        let p = qp(function_header, "fn foo<'a, 'b>()");
        assert_eq!(unwrap_progress(p).extent, (0, 16))
    }

    #[test]
    fn fn_with_lifetimes_and_generics() {
        let p = qp(function_header, "fn foo<'a, T>()");
        assert_eq!(unwrap_progress(p).extent, (0, 15))
    }

    #[test]
    fn block_promotes_implicit_statement_to_expression() {
        let p = qp(block, "{ if a {} }");
        let p = unwrap_progress(p);
        assert!(p.statements.is_empty());
        assert_eq!(p.expression.unwrap().extent, (2, 9));
    }

    #[test]
    fn statement_match_no_semicolon() {
        let p = qp(statement, "match a { _ => () }");
        assert_eq!(unwrap_progress(p).implicit().unwrap().extent, (0, 19))
    }

    #[test]
    fn statement_use() {
        let p = qp(statement, "use foo::Bar;");
        assert_eq!(unwrap_progress(p).extent(), (0, 13))
    }

    #[test]
    fn expr_true() {
        let p = qp(expression, "true");
        assert_eq!(unwrap_progress(p).extent, (0, 4))
    }

    #[test]
    fn expr_let_explicit_type() {
        let p = qp(expression, "let foo: bool");
        assert_eq!(unwrap_progress(p).extent, (0, 13))
    }

    #[test]
    fn expr_let_mut() {
        let p = qp(expression, "let mut pm = Master::new()");
        assert_eq!(unwrap_progress(p).extent, (0, 26))
    }

    #[test]
    fn expr_let_no_value() {
        let p = qp(expression, "let pm");
        assert_eq!(unwrap_progress(p).extent, (0, 6))
    }

    #[test]
    fn expr_assign() {
        let p = qp(expression, "a = b");
        assert_eq!(unwrap_progress(p).extent, (0, 5))
    }

    #[test]
    fn expr_value_with_path() {
        let p = qp(expression, "Master::new()");
        assert_eq!(unwrap_progress(p).extent, (0, 13))
    }

    #[test]
    fn expr_field_access() {
        let p = qp(expression, "foo.bar");
        assert_eq!(unwrap_progress(p).extent, (0, 7))
    }

    #[test]
    fn expr_field_access_multiple() {
        let p = qp(expression, "foo.bar.baz");
        assert_eq!(unwrap_progress(p).extent, (0, 11))
    }

    #[test]
    fn expr_function_call() {
        let p = qp(expression, "foo()");
        assert_eq!(unwrap_progress(p).extent, (0, 5))
    }

    #[test]
    fn pathed_ident_with_turbofish() {
        let p = qp(pathed_ident, "foo::<Vec<u8>>");
        assert_eq!(unwrap_progress(p), (0, 14))
    }

    #[test]
    fn expr_function_call_with_args() {
        let p = qp(expression, "foo(true)");
        assert_eq!(unwrap_progress(p).extent, (0, 9))
    }

    #[test]
    fn expr_method_call() {
        let p = qp(expression, "foo.bar()");
        assert_eq!(unwrap_progress(p).extent, (0, 9))
    }

    #[test]
    fn expr_method_call_multiple() {
        let p = qp(expression, "foo.bar().baz()");
        assert_eq!(unwrap_progress(p).extent, (0, 15))
    }

    #[test]
    fn expr_method_call_multiple_spaced() {
        let p = qp(expression, "foo.bar()\n    .baz()");
        assert_eq!(unwrap_progress(p).extent, (0, 20))
    }

    #[test]
    fn expr_method_call_with_turbofish() {
        let p = qp(expression, "foo.bar::<u8>()");
        assert_eq!(unwrap_progress(p).extent, (0, 15))
    }

    #[test]
    fn expr_method_call_with_turbofish_nested() {
        let p = qp(expression, "e.into_iter().collect::<BTreeSet<_>>()");
        assert_eq!(unwrap_progress(p).extent, (0, 38))
    }

    #[test]
    fn expr_call_of_expr() {
        let p = qp(expression, "{foo}()");
        assert_eq!(unwrap_progress(p).extent, (0, 7))
    }

    #[test]
    fn expr_for_loop() {
        let p = qp(expression, "for (a, b) in c {}");
        assert_eq!(unwrap_progress(p).extent, (0, 18))
    }

    #[test]
    fn expr_loop() {
        let p = qp(expression, "loop {}");
        assert_eq!(unwrap_progress(p).extent, (0, 7))
    }

    #[test]
    fn expr_match() {
        let p = qp(expression, "match foo { _ => () }");
        assert_eq!(unwrap_progress(p).extent, (0, 21))
    }

    #[test]
    fn expr_tuple() {
        let p = qp(expression, "(1, 2)");
        assert_eq!(unwrap_progress(p).extent, (0, 6))
    }

    #[test]
    fn expr_block() {
        let p = qp(expression, "{}");
        assert_eq!(unwrap_progress(p).extent, (0, 2))
    }

    #[test]
    fn expr_if_() {
        let p = qp(expression, "if a {}");
        assert_eq!(unwrap_progress(p).extent, (0, 7))
    }

    #[test]
    fn expr_if_else() {
        let p = qp(expression, "if a {} else {}");
        assert_eq!(unwrap_progress(p).extent, (0, 15))
    }

    #[test]
    fn expr_if_else_if() {
        let p = qp(expression, "if a {} else if b {}");
        assert_eq!(unwrap_progress(p).extent, (0, 20))
    }

    #[test]
    fn expr_binary_op() {
        let p = qp(expression, "a < b");
        assert_eq!(unwrap_progress(p).extent, (0, 5))
    }

    #[test]
    fn expr_binary_multiple() {
        let p = qp(expression, "1 + 2 + 3");
        assert_eq!(unwrap_progress(p).extent, (0, 9))
    }

    #[test]
    fn expr_binary_op_two_char() {
        let p = qp(expression, "a >= b");
        assert_eq!(unwrap_progress(p).extent, (0, 6))
    }

    #[test]
    fn expr_binary_op_equality() {
        let p = qp(expression, "a == b != c");
        assert_eq!(unwrap_progress(p).extent, (0, 11))
    }

    #[test]
    fn expr_binary_op_boolean_logic() {
        let p = qp(expression, "a && b || c");
        assert_eq!(unwrap_progress(p).extent, (0, 11))
    }

    #[test]
    fn expr_braced_true() {
        let p = qp(expression, "{ true }");
        assert_eq!(unwrap_progress(p).extent, (0, 8))
    }

    #[test]
    fn expr_macro_call_with_nested_parens() {
        let p = qp(expression, "foo!(())");
        assert_eq!(unwrap_progress(p).extent, (0, 8))
    }

    #[test]
    fn expr_macro_call_with_square_brackets() {
        let p = qp(expression, "vec![]");
        assert_eq!(unwrap_progress(p).extent, (0, 6))
    }

    #[test]
    fn expr_range_both() {
        let p = qp(expression, "1..2");
        assert_eq!(unwrap_progress(p).extent, (0, 4))
    }

    #[test]
    fn expr_range_left() {
        let p = qp(expression, "3..");
        assert_eq!(unwrap_progress(p).extent, (0, 3))
    }

    #[test]
    fn expr_range_right() {
        let p = qp(expression, "..4");
        assert_eq!(unwrap_progress(p).extent, (0, 3))
    }

    #[test]
    fn expr_range_none() {
        let p = qp(expression, "..");
        assert_eq!(unwrap_progress(p).extent, (0, 2))
    }

    #[test]
    fn expr_value_struct_literal() {
        let p = qp(expression, "Point { a: 1 }");
        assert_eq!(unwrap_progress(p).extent, (0, 14))
    }

    #[test]
    fn expr_value_struct_literal_shortahnd() {
        let p = qp(expression, "Point { a }");
        assert_eq!(unwrap_progress(p).extent, (0, 11))
    }

    #[test]
    fn expr_closure() {
        let p = qp(expression, "|a| a");
        assert_eq!(unwrap_progress(p).extent, (0, 5))
    }

    #[test]
    fn expr_closure_multiple() {
        let p = qp(expression, "|a, b| a + b");
        assert_eq!(unwrap_progress(p).extent, (0, 12))
    }

    #[test]
    fn expr_closure_explicit_type() {
        let p = qp(expression, "|a: u8| a");
        assert_eq!(unwrap_progress(p).extent, (0, 9))
    }

    #[test]
    fn expr_closure_move() {
        let p = qp(expression, "move || 42");
        assert_eq!(unwrap_progress(p).extent, (0, 10))
    }

    #[test]
    fn expr_return() {
        let p = qp(expression, "return 1");
        assert_eq!(unwrap_progress(p).extent, (0, 8))
    }

    #[test]
    fn expr_array() {
        let p = qp(expression, "[1, 1]");
        assert_eq!(unwrap_progress(p).extent, (0, 6))
    }

    #[test]
    fn expr_char_literal() {
        let p = qp(expression, "'a'");
        assert_eq!(unwrap_progress(p).extent, (0, 3))
    }

    #[test]
    fn expr_char_literal_escape() {
        let p = qp(expression, r"'\''");
        assert_eq!(unwrap_progress(p).extent, (0, 4))
    }

    #[test]
    fn expr_string_literal() {
        let p = qp(expression, r#""a""#);
        assert_eq!(unwrap_progress(p).extent, (0, 3))
    }

    #[test]
    fn expr_string_literal_escape() {
        let p = qp(expression, r#""\"""#);
        assert_eq!(unwrap_progress(p).extent, (0, 4))
    }

    #[test]
    fn expr_string_literal_raw() {
        let p = qp(expression, r###"r#"foo"#"###);
        assert_eq!(unwrap_progress(p).extent, (0, 8))
    }

    #[test]
    fn expr_slice_index() {
        let p = qp(expression, "a[..2]");
        assert_eq!(unwrap_progress(p).extent, (0, 6))
    }

    #[test]
    fn pattern_with_path() {
        let p = qp(pattern, "foo::Bar::Baz");
        assert_eq!(unwrap_progress(p).extent(), (0, 13))
    }

    #[test]
    fn pattern_with_tuple() {
        let p = qp(pattern, "(a, b)");
        assert_eq!(unwrap_progress(p).extent(), (0, 6))
    }

    #[test]
    fn pattern_with_enum_tuple() {
        let p = qp(pattern, "Baz(a)");
        assert_eq!(unwrap_progress(p).extent(), (0, 6))
    }

    #[test]
    fn pattern_with_tuple_wildcard() {
        let p = qp(pattern, "(..)");
        assert_eq!(unwrap_progress(p).extent(), (0, 4))
    }

    #[test]
    fn pattern_with_enum_struct() {
        let p = qp(pattern, "Baz { a: a }");
        assert_eq!(unwrap_progress(p).extent(), (0, 12))
    }

    #[test]
    fn pattern_with_enum_struct_shorthand() {
        let p = qp(pattern, "Baz { a }");
        assert_eq!(unwrap_progress(p).extent(), (0, 9))
    }

    #[test]
    fn pattern_with_enum_struct_wildcard() {
        let p = qp(pattern, "Baz { .. }");
        assert_eq!(unwrap_progress(p).extent(), (0, 10))
    }

    #[test]
    fn pattern_with_char_literal() {
        let p = qp(pattern, "'a'");
        assert_eq!(unwrap_progress(p).extent(), (0, 3))
    }

    #[test]
    fn match_arm_with_alternate() {
        let p = qp(match_arm, "a | b => 1");
        assert_eq!(unwrap_progress(p).extent, (0, 10))
    }

    #[test]
    fn type_tuple() {
        let p = qp(typ, "(u8, u8)");
        assert_eq!(unwrap_progress(p), (0, 8))
    }

    #[test]
    fn type_with_generics() {
        let p = qp(typ, "A<T>");
        assert_eq!(unwrap_progress(p), (0, 4))
    }

    #[test]
    fn type_impl_trait() {
        let p = qp(typ, "impl Foo");
        assert_eq!(unwrap_progress(p), (0, 8))
    }

    #[test]
    fn type_fn_trait() {
        let p = qp(typ, "Fn(u8) -> u8");
        assert_eq!(unwrap_progress(p), (0, 12))
    }

    #[test]
    fn type_mut_ref() {
        let p = qp(typ, "&mut Foo");
        assert_eq!(unwrap_progress(p), (0, 8))
    }

    #[test]
    fn struct_basic() {
        let p = qp(p_struct, "struct S { field: TheType, other: OtherType }");
        assert_eq!(unwrap_progress(p).extent, (0, 45))
    }

    #[test]
    fn struct_with_generic_fields() {
        let p = qp(p_struct, "struct S { field: Option<u8> }");
        assert_eq!(unwrap_progress(p).extent, (0, 30))
    }

    #[test]
    fn struct_public() {
        let p = qp(p_struct, "pub struct S {}");
        assert_eq!(unwrap_progress(p).extent, (0, 15))
    }

    #[test]
    fn where_clause_with_path() {
        let p = qp(function_where, "P: std::str::pattern::Pattern<'s>");
        assert_eq!(unwrap_progress(p).extent, (0, 33))
    }

    #[test]
    fn where_clause_with_multiple_bounds() {
        let p = qp(function_where, "P: A + B");
        assert_eq!(unwrap_progress(p).extent, (0, 8))
    }

    #[test]
    fn where_clause_with_multiple_types() {
        let p = qp(function_where_clause, "where P: A, Q: B");
        let p = unwrap_progress(p);
        assert_eq!(p[1].extent, (12, 16))
    }

    #[test]
    fn comment_end_of_line() {
        let p = qp(comment, "// hello");
        assert_eq!(unwrap_progress(p).extent, (0, 8))
    }

    #[test]
    fn comment_region() {
        let p = qp(comment, "/* hello */");
        assert_eq!(unwrap_progress(p).extent, (0, 11))
    }

    fn unwrap_progress<P, T, E>(p: peresil::Progress<P, T, E>) -> T
        where P: std::fmt::Debug,
              E: std::fmt::Debug,
    {
        match p {
            peresil::Progress { status: peresil::Status::Success(v), .. } => v,
            peresil::Progress { status: peresil::Status::Failure(e), point } => {
                panic!("Failed parsing at {:?}: {:?}", point, e)
            }
        }
    }
}
