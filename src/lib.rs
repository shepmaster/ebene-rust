#![feature(field_init_shorthand)]
#![feature(conservative_impl_trait)]
#![feature(pattern)]
#![feature(custom_derive)]

#[macro_use]
extern crate visit_derive;

#[macro_use]
extern crate peresil;

extern crate unicode_xid;

use std::collections::BTreeSet;
use unicode_xid::UnicodeXID;
use peresil::combinators::*;

// define what you want to parse; likely a string
// create an error type
// definte type aliases
type Point<'s> = peresil::StringPoint<'s>;
type Master<'s> = peresil::ParseMaster<Point<'s>, Error>;
type Progress<'s, T> = peresil::Progress<Point<'s>, T, Error>;

// define an error type - emphasis on errors. Need to implement Recoverable (more to discuss.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Error {
    Literal(&'static str),
    IdentNotFound,
    NumberNotFound,
    UnterminatedRawString,
}

impl peresil::Recoverable for Error {
    fn recoverable(&self) -> bool { true }
}

#[derive(Debug, PartialEq)]
pub struct ErrorDetail {
    location: usize,
    errors: BTreeSet<Error>,
}

impl ErrorDetail {
    pub fn with_text<'a>(&'a self, text: &'a str) -> ErrorDetailText<'a> {
        ErrorDetailText { detail: self, text }
    }
}

#[derive(Debug)]
pub struct ErrorDetailText<'a> {
    detail: &'a ErrorDetail,
    text: &'a str,
}

use std::fmt;

impl<'a> fmt::Display for ErrorDetailText<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (head, tail) = self.text.split_at(self.detail.location);
        let start_of_line = head.rfind("\n").unwrap_or(0);
        let end_of_line = tail.find("\n").unwrap_or_else(|| tail.len());

        let head_line = &head[start_of_line..];
        let tail_line = &tail[..end_of_line];

        let line = head.matches("\n").count() + 1; // Normally the first line is #1, so add one
        let col = head_line.len();

        writeln!(f, "Unable to parse text (line {}, column {})", line, col)?;
        writeln!(f, "{}{}", head_line, tail_line)?;
        writeln!(f, "{:>width$}", "^", width = col)?;
        writeln!(f, "Expected:")?;
        for e in &self.detail.errors {
            writeln!(f, "  {:?}", e)?; // TODO: should be Display
        }
        Ok(())
    }
}

// Construct a point, initialize  the master. This is what stores errors
// todo: rename?

pub fn parse_rust_file(file: &str) -> Result<File, ErrorDetail> {
    let mut pt = Point::new(file);
    let mut pm = Master::new();
    let mut items = Vec::new();

    loop {
        let next_pt;

        let item = item(&mut pm, pt);
        let item = pm.finish(item);

        match item.status {
            peresil::Status::Success(s) => {
                items.push(s);
                next_pt = item.point;
            },
            peresil::Status::Failure(e) => {
                return Err(ErrorDetail {
                    location: item.point.offset,
                    errors: e.into_iter().collect(),
                })
            },
        }

        if next_pt.offset <= pt.offset {
            let end = std::cmp::min(pt.offset + 10, file.len());
            panic!("Could not make progress: {}...", &file[pt.offset..end]);
        }
        pt = next_pt;

        if pt.s.is_empty() { break }
    }

    Ok(File { items: items })

    // TODO: add `expect` to progress?
}

// TODO: enum variants track whole extent, enum delegates

pub type Extent = (usize, usize);

#[derive(Debug, Visit)]
pub struct File {
    items: Vec<Item>,
}

#[derive(Debug, Visit)]
pub enum Item {
    Attribute(Attribute),
    Const(Const),
    Enum(Enum),
    ExternCrate(Crate),
    Function(Function),
    Impl(Impl),
    MacroRules(MacroRules),
    Module(Module),
    Struct(Struct),
    Trait(Trait),
    TypeAlias(TypeAlias),
    Use(Use),
    Whitespace(Vec<Whitespace>),
}

impl Item {
    #[allow(dead_code)]
    pub fn extent(&self) -> Extent {
        match *self {
            Item::Attribute(Attribute { extent, .. })   |
            Item::Const(Const { extent, .. })           |
            Item::Enum(Enum { extent, .. })             |
            Item::ExternCrate(Crate { extent, .. })     |
            Item::Function(Function { extent, .. })     |
            Item::Impl(Impl { extent, .. })             |
            Item::MacroRules(MacroRules { extent, .. }) |
            Item::Module(Module { extent, .. })         |
            Item::Struct(Struct { extent, .. })         |
            Item::Trait(Trait { extent, .. })           |
            Item::TypeAlias(TypeAlias { extent, .. })   |
            Item::Use(Use { extent, .. })               => extent,
            Item::Whitespace(..)                        => unimplemented!(),
        }
    }
}

#[derive(Debug, Visit)]
pub struct Attribute {
    extent: Extent,
    is_containing: Option<Extent>,
    text: Extent,
}

#[derive(Debug, Visit)]
pub struct Lifetime {
    extent: Extent,
    name: Ident,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub enum Whitespace {
    Comment(Comment),
    Whitespace(Extent),
}

#[derive(Debug, Visit)]
pub struct Comment {
    extent: Extent,
    text: Extent,
}

#[derive(Debug, Visit)]
pub struct Use {
    extent: Extent,
    path: Vec<Ident>,
    tail: UseTail,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub enum UseTail {
    Ident(UseTailIdent),
    Glob(UseTailGlob),
    Multi(UseTailMulti),
}

#[derive(Debug, Visit)]
pub struct UseTailIdent {
    name: Ident,
}

#[derive(Debug, Visit)]
pub struct UseTailGlob {
    extent: Extent,
}

#[derive(Debug, Visit)]
pub struct UseTailMulti {
    extent: Extent,
    names: Vec<Ident>,
}

#[derive(Debug, Visit)]
pub struct Function {
    pub extent: Extent,
    pub header: FunctionHeader,
    body: Block,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct FunctionHeader {
    pub extent: Extent,
    visibility: Option<Visibility>,
    pub name: Ident,
    generics: Option<GenericDeclarations>,
    arguments: Vec<Argument>,
    return_type: Option<Type>,
    wheres: Vec<Where>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct TraitImplFunctionHeader {
    extent: Extent,
    visibility: Option<Visibility>,
    pub name: Ident,
    generics: Option<GenericDeclarations>,
    arguments: Vec<TraitImplArgument>,
    return_type: Option<Type>,
    wheres: Vec<Where>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct GenericDeclarations {
    pub extent: Extent,
    lifetimes: Vec<Lifetime>,
    types: Vec<Generic>,
}

#[derive(Debug, Visit)]
pub struct MacroRules {
    extent: Extent,
    name: Ident,
    body: Extent,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Generic {
    extent: Extent,
    name: Ident,
}

#[derive(Debug, Visit)]
pub struct Type {
    extent: Extent,
    reference: Option<TypeReference>,
    inner: TypeInner,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct TypeReference {
    extent: Extent,
    lifetime: Option<Lifetime>,
    mutable: Option<Extent>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub enum TypeInner {
    Core(TypeCore),
    Tuple(Vec<Type>),
}

#[derive(Debug, Visit)]
pub struct TypeCore {
    extent: Extent,
    is_impl: Option<Extent>,
    name: PathedIdent,
    generics: Option<TypeGenerics>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub enum TypeGenerics {
    Function(TypeGenericsFunction),
    Angle(TypeGenericsAngle),
}

#[derive(Debug, Visit)]
pub struct TypeGenericsFunction {
    extent: Extent,
    types: Vec<Type>,
    return_type: Option<Box<Type>>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct TypeGenericsAngle {
    extent: Extent,
    lifetimes: Vec<Lifetime>,
    types: Vec<Type>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Copy, Clone, Visit)]
pub struct Ident {
    pub extent: Extent,
}

// TODO: Can we reuse the path from the `use` statement?
#[derive(Debug, Visit)]
pub struct PathedIdent {
    extent: Extent,
    idents: Vec<Ident>,
    turbofish: Option<Turbofish>,
}

#[derive(Debug, Visit)]
pub struct Turbofish {
    extent: Extent,
    types: Vec<Type>,
}

impl From<Ident> for PathedIdent {
    fn from(other: Ident) -> PathedIdent {
        PathedIdent { extent: other.extent, idents: vec![other], turbofish: None }
    }
}

#[derive(Debug, Visit)]
pub struct Const {
    extent: Extent,
    visibility: Option<Visibility>,
    name: Ident,
    typ: Type,
    value: Expression,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Struct {
    pub extent: Extent,
    visibility: Option<Visibility>,
    name: Ident,
    generics: Option<GenericDeclarations>,
    body: StructDefinitionBody,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct StructDefinitionBody {
    pub extent: Extent,
    fields: Vec<StructField>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct StructField {
    extent: Extent,
    attributes: Vec<Attribute>,
    visibility: Option<Visibility>,
    name: Ident,
    typ: Type,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Enum {
    pub extent: Extent,
    visibility: Option<Visibility>,
    name: Ident,
    generics: Option<GenericDeclarations>,
    variants: Vec<EnumVariant>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct EnumVariant {
    extent: Extent,
    name: Ident,
    body: Option<EnumVariantBody>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub enum EnumVariantBody {
    Tuple(Vec<Type>),
    Struct(StructDefinitionBody),
}

#[derive(Debug, Visit)]
pub enum Argument {
    SelfArgument(SelfArgument),
    Named(NamedArgument),
}

#[derive(Debug, Visit)]
pub struct SelfArgument {
    extent: Extent,
    reference: Option<TypeReference>,
    name: Ident,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct NamedArgument {
    name: Pattern,
    typ: Type,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub enum TraitImplArgument {
    SelfArgument(SelfArgument),
    Named(TraitImplArgumentNamed),
}

#[derive(Debug, Visit)]
pub struct TraitImplArgumentNamed {
    name: Option<Pattern>,
    typ: Type,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Where {
    pub extent: Extent,
    name: Type,
    bounds: Vec<Type>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Block {
    extent: Extent,
    statements: Vec<Statement>,
    expression: Option<Expression>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub enum Statement {
    Explicit(Expression),
    Implicit(Expression),
    Use(Use),
}

impl Statement {
    #[allow(dead_code)]
    pub fn extent(&self) -> Extent {
        use Statement::*;
        match *self {
            Explicit(ref e) |
            Implicit(ref e) => e.extent(),
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

#[derive(Debug, Visit)]
pub enum Expression {
    Array(Array),
    Binary(Binary),
    Block(Box<Block>),
    Call(Call),
    Character(Character),
    Closure(Closure),
    Dereference(Dereference),
    FieldAccess(FieldAccess),
    ForLoop(ForLoop),
    FunctionCall(FunctionCall),
    If(If),
    Let(Let),
    Loop(Loop),
    MacroCall(MacroCall),
    Match(Match),
    MethodCall(MethodCall),
    Number(Number),
    Range(Range),
    Reference(Reference),
    Return(Return),
    Slice(Slice),
    String(String),
    Tuple(Tuple),
    TryOperator(TryOperator),
    Unary(Unary),
    Value(Value),
}

impl Expression {
    pub fn extent(&self) -> Extent {
        match *self {
            Expression::Block(ref x) => x.extent,

            Expression::Array(Array { extent, .. }) |
            Expression::Binary(Binary { extent, .. }) |
            Expression::Call(Call { extent, .. }) |
            Expression::Character(Character { extent, .. }) |
            Expression::Closure(Closure { extent, .. }) |
            Expression::Dereference(Dereference { extent, .. }) |
            Expression::FieldAccess(FieldAccess { extent, .. }) |
            Expression::ForLoop(ForLoop { extent, .. }) |
            Expression::FunctionCall(FunctionCall { extent, .. }) |
            Expression::If(If { extent, .. }) |
            Expression::Let(Let { extent, .. }) |
            Expression::Loop(Loop { extent, .. }) |
            Expression::MacroCall(MacroCall { extent, .. }) |
            Expression::Match(Match { extent, .. }) |
            Expression::MethodCall(MethodCall { extent, .. }) |
            Expression::Number(Number { extent, .. }) => extent,
            Expression::Range(Range { extent, .. }) |
            Expression::Reference(Reference { extent, .. }) |
            Expression::Return(Return { extent, .. }) |
            Expression::Slice(Slice { extent, .. }) |
            Expression::String(String { extent, .. }) |
            Expression::TryOperator(TryOperator { extent, .. }) |
            Expression::Tuple(Tuple { extent, .. }) |
            Expression::Unary(Unary { extent, .. }) |
            Expression::Value(Value { extent, .. }) => extent,
        }
    }
}

#[derive(Debug, Visit)]
pub struct MacroCall {
    extent: Extent,
    name: Ident,
    args: Extent,
}

#[derive(Debug, Visit)]
pub struct Let {
    extent: Extent,
    pattern: Pattern,
    typ: Option<Type>,
    value: Option<Box<Expression>>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Tuple {
    extent: Extent,
    members: Vec<Expression>,
}

#[derive(Debug, Visit)]
pub struct TryOperator {
    extent: Extent,
    target: Box<Expression>,
}

#[derive(Debug, Visit)]
pub struct FieldAccess {
    extent: Extent,
    value: Box<Expression>,
    field: FieldName,
}

#[derive(Debug)]
pub enum FieldName {
    Ident(Ident),
    Number(Extent),
}

#[derive(Debug, Visit)]
pub struct Number {
    extent: Extent,
}

#[derive(Debug, Visit)]
pub struct Value {
    extent: Extent,
    name: PathedIdent,
    literal: Option<Vec<StructLiteralField>>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct StructLiteralField {
    name: Ident,
    value: Expression,
}

// TODO: Can we roll up function and method call into this?
#[derive(Debug, Visit)]
pub struct Call {
    extent: Extent,
    target: Box<Expression>,
    args: Vec<Expression>,
}

#[derive(Debug, Visit)]
pub struct FunctionCall {
    extent: Extent,
    name: PathedIdent,
    args: Vec<Expression>,
}

#[derive(Debug, Visit)]
pub struct MethodCall {
    extent: Extent,
    receiver: Box<Expression>,
    name: Ident,
    turbofish: Option<Turbofish>,
    args: Vec<Expression>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct ForLoop {
    extent: Extent,
    pattern: Pattern,
    iter: Box<Expression>,
    body: Box<Block>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Loop {
    extent: Extent,
    body: Box<Block>,
    whitespace: Vec<Whitespace>,
}

// TODO: Should this be the same as dereference? What about reference?
#[derive(Debug, Visit)]
pub struct Unary {
    extent: Extent,
    op: Extent,
    value: Box<Expression>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Binary {
    extent: Extent,
    op: BinaryOp,
    lhs: Box<Expression>,
    rhs: Box<Expression>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    AddAssign,
    Assign,
    BooleanAnd,
    BooleanOr,
    Div,
    DivAssign,
    Equal,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    Mod,
    ModAssign,
    Mul,
    MulAssign,
    NotEqual,
    Sub,
    SubAssign,
}

#[derive(Debug, Visit)]
pub struct If {
    extent: Extent,
    condition: Box<Expression>,
    body: Box<Block>,
    more: Vec<If>,
    else_body: Option<Box<Block>>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Match {
    extent: Extent,
    head: Box<Expression>,
    arms: Vec<MatchArm>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct MatchArm {
    extent: Extent,
    pattern: Vec<Pattern>,
    body: Expression,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Range {
    extent: Extent,
    lhs: Option<Box<Expression>>,
    rhs: Option<Box<Expression>>,
}

#[derive(Debug, Visit)]
pub struct Array {
    extent: Extent,
    members: Vec<Expression>,
}

#[derive(Debug, Visit)]
pub struct Character {
    extent: Extent,
    value: Extent,
}

#[derive(Debug, Visit)]
pub struct String {
    extent: Extent,
    value: Extent,
}

#[derive(Debug, Visit)]
pub struct Slice {
    extent: Extent,
    target: Box<Expression>,
    range: Box<Expression>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Closure {
    extent: Extent,
    #[visit(ignore)]
    is_move: bool,
    args: Vec<ClosureArg>,
    body: Box<Expression>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct ClosureArg {
    name: Pattern,
    typ: Option<Type>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Reference {
    extent: Extent,
    mutable: Option<Extent>,
    value: Box<Expression>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Dereference {
    extent: Extent,
    value: Box<Expression>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Return {
    extent: Extent,
    value: Box<Expression>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug)]
enum ExpressionTail {
    Binary { op: BinaryOp, rhs: Box<Expression>, whitespace: Vec<Whitespace> },
    FieldAccess { field: FieldName },
    Call { args: Vec<Expression> },
    MethodCall {
        name: Ident,
        turbofish: Option<Turbofish>,
        args: Vec<Expression>,
        whitespace: Vec<Whitespace>,
    },
    Range { rhs: Option<Box<Expression>> },
    Slice { range: Box<Expression>, whitespace: Vec<Whitespace> },
    TryOperator,
}

#[derive(Debug, Visit)]
pub enum Pattern {
    Character(PatternCharacter),
    Ident(PatternIdent), // TODO: split into ident and enumtuple
    Number(PatternNumber),
    Ref(PatternRef),
    Reference(PatternReference),
    String(PatternString),
    Struct(PatternStruct),
    Tuple(PatternTuple),
    Wildcard(PatternWildcard),
}

impl Pattern {
    #[allow(dead_code)]
    fn extent(&self) -> Extent {
        match *self {
            Pattern::Character(PatternCharacter { extent, .. }) |
            Pattern::Ident(PatternIdent { extent, .. })         |
            Pattern::Number(PatternNumber { extent, .. })       |
            Pattern::Ref(PatternRef { extent, .. })             |
            Pattern::Reference(PatternReference { extent, .. }) |
            Pattern::String(PatternString { extent, .. })       |
            Pattern::Struct(PatternStruct { extent, .. })       |
            Pattern::Tuple(PatternTuple { extent, .. })         |
            Pattern::Wildcard(PatternWildcard { extent, .. })   => extent
        }
    }
}

#[derive(Debug, Visit)]
pub struct PatternIdent {
    extent: Extent,
    mutable: Option<Extent>,
    ident: PathedIdent,
    tuple: Vec<Pattern>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct PatternStruct {
    extent: Extent,
    name: PathedIdent,
    fields: Vec<PatternStructField>,
    #[visit(ignore)]
    wildcard: bool,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct PatternStructField {
    name: Ident,
    pattern: Pattern,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct PatternTuple {
    extent: Extent,
    members: Vec<Pattern>,
}

#[derive(Debug, Visit)]
pub struct PatternWildcard {
    extent: Extent,
}

#[derive(Debug, Visit)]
pub struct PatternCharacter {
    extent: Extent,
    value: Character,
}

#[derive(Debug, Visit)]
pub struct PatternString {
    extent: Extent,
    value: String,
}

#[derive(Debug, Visit)]
pub struct PatternNumber {
    extent: Extent,
    value: Number,
}

// TODO: Should we actually have a "qualifier" that applies to all
// patterns equally? Could include `mut` in there too...
#[derive(Debug, Visit)]
pub struct PatternRef {
    extent: Extent,
    pattern: Box<Pattern>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct PatternReference {
    extent: Extent,
    pattern: Box<Pattern>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Trait {
    extent: Extent,
    visibility: Option<Visibility>,
    name: Ident,
    generics: Option<GenericDeclarations>,
    members: Vec<TraitMember>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub enum TraitMember {
    Function(TraitImplFunction),
    Attribute(Attribute),
    Whitespace(Vec<Whitespace>),
}

#[derive(Debug, Visit)]
pub struct TraitImplFunction {
    extent: Extent,
    header: TraitImplFunctionHeader,
    body: Option<Block>,
}

#[derive(Debug, Visit)]
pub struct Impl {
    extent: Extent,
    generics: Option<GenericDeclarations>,
    trait_name: Option<Type>,
    type_name: Type,
    wheres: Vec<Where>,
    body: Vec<ImplMember>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub enum ImplMember {
    Function(ImplFunction),
    Attribute(Attribute),
    Whitespace(Vec<Whitespace>),
}

#[derive(Debug, Visit)]
pub struct ImplFunction {
    extent: Extent,
    header: FunctionHeader,
    body: Block,
}

#[derive(Debug, Visit)]
pub struct Crate {
    extent: Extent,
    name: Ident,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct TypeAlias {
    extent: Extent,
    visibility: Option<Visibility>,
    name: Type,
    defn: Type,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Module {
    extent: Extent,
    name: Ident,
    body: Vec<Item>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Visibility {
    extent: Extent,
    whitespace: Vec<Whitespace>,
}

// --------------------------------------------------

pub trait Visit {
    fn visit<V>(&self, &mut V)
        where V: Visitor;
}

impl<T> Visit for Box<T>
    where T: Visit
{
    fn visit<V>(&self, v: &mut V)
        where V: Visitor
    {
        (**self).visit(v)
    }
}

impl<T> Visit for Option<T>
    where T: Visit
{
    fn visit<V>(&self, v: &mut V)
        where V: Visitor
    {
        for i in self {
            i.visit(v)
        }
    }
}

impl<T> Visit for Vec<T>
    where T: Visit
{
    fn visit<V>(&self, v: &mut V)
        where V: Visitor
    {
        for i in self {
            i.visit(v)
        }
    }
}

// Cheap hacks to avoid having to annotate every terminal `Extent` and
// enum; just visit them and don't do anything.

// An extent without any context is pretty useless.
impl Visit for Extent {
    fn visit<V>(&self, _v: &mut V)
        where V: Visitor
    {}
}

// Can't imagine we'd ever want to count the number of additions;
// without the lhs/rhs there's not much benefit.
impl Visit for BinaryOp {
    fn visit<V>(&self, _v: &mut V)
        where V: Visitor
    {}
}

// We *might* want to visit this, to enable checking for "large" tuple
// indexes or poor variable names?
impl Visit for FieldName {
    fn visit<V>(&self, _v: &mut V)
        where V: Visitor
    {}
}

pub trait Visitor {
    fn visit_argument(&mut self, &Argument) {}
    fn visit_array(&mut self, &Array) {}
    fn visit_attribute(&mut self, &Attribute) {}
    fn visit_binary(&mut self, &Binary) {}
    fn visit_block(&mut self, &Block) {}
    fn visit_call(&mut self, &Call) {}
    fn visit_character(&mut self, &Character) {}
    fn visit_closure(&mut self, &Closure) {}
    fn visit_closure_arg(&mut self, &ClosureArg) {}
    fn visit_comment(&mut self, &Comment) {}
    fn visit_const(&mut self, &Const) {}
    fn visit_crate(&mut self, &Crate) {}
    fn visit_dereference(&mut self, &Dereference) {}
    fn visit_enum(&mut self, &Enum) {}
    fn visit_enum_variant(&mut self, &EnumVariant) {}
    fn visit_enum_variant_body(&mut self, &EnumVariantBody) {}
    fn visit_expression(&mut self, &Expression) {}
    fn visit_field_access(&mut self, &FieldAccess) {}
    fn visit_file(&mut self, &File) {}
    fn visit_for_loop(&mut self, &ForLoop) {}
    fn visit_function(&mut self, &Function) {}
    fn visit_function_call(&mut self, &FunctionCall) {}
    fn visit_function_header(&mut self, &FunctionHeader) {}
    fn visit_generic(&mut self, &Generic) {}
    fn visit_generic_declarations(&mut self, &GenericDeclarations) {}
    fn visit_ident(&mut self, &Ident) {}
    fn visit_if(&mut self, &If) {}
    fn visit_impl(&mut self, &Impl) {}
    fn visit_impl_function(&mut self, &ImplFunction) {}
    fn visit_impl_member(&mut self, &ImplMember) {}
    fn visit_item(&mut self, &Item) {}
    fn visit_let(&mut self, &Let) {}
    fn visit_lifetime(&mut self, &Lifetime) {}
    fn visit_loop(&mut self, &Loop) {}
    fn visit_macro_call(&mut self, &MacroCall) {}
    fn visit_macro_rules(&mut self, &MacroRules) {}
    fn visit_match(&mut self, &Match) {}
    fn visit_match_arm(&mut self, &MatchArm) {}
    fn visit_method_call(&mut self, &MethodCall) {}
    fn visit_module(&mut self, &Module) {}
    fn visit_named_argument(&mut self, &NamedArgument) {}
    fn visit_number(&mut self, &Number) {}
    fn visit_pathed_ident(&mut self, &PathedIdent) {}
    fn visit_pattern(&mut self, &Pattern) {}
    fn visit_pattern_character(&mut self, &PatternCharacter) {}
    fn visit_pattern_ident(&mut self, &PatternIdent) {}
    fn visit_pattern_number(&mut self, &PatternNumber) {}
    fn visit_pattern_ref(&mut self, &PatternRef) {}
    fn visit_pattern_reference(&mut self, &PatternReference) {}
    fn visit_pattern_string(&mut self, &PatternString) {}
    fn visit_pattern_struct(&mut self, &PatternStruct) {}
    fn visit_pattern_struct_field(&mut self, &PatternStructField) {}
    fn visit_pattern_tuple(&mut self, &PatternTuple) {}
    fn visit_pattern_wildcard(&mut self, &PatternWildcard) {}
    fn visit_range(&mut self, &Range) {}
    fn visit_reference(&mut self, &Reference) {}
    fn visit_return(&mut self, &Return) {}
    fn visit_self_argument(&mut self, &SelfArgument) {}
    fn visit_slice(&mut self, &Slice) {}
    fn visit_statement(&mut self, &Statement) {}
    fn visit_string(&mut self, &String) {}
    fn visit_struct(&mut self, &Struct) {}
    fn visit_struct_definition_body(&mut self, &StructDefinitionBody) {}
    fn visit_struct_field(&mut self, &StructField) {}
    fn visit_struct_literal_field(&mut self, &StructLiteralField) {}
    fn visit_trait(&mut self, &Trait) {}
    fn visit_trait_impl_argument(&mut self, &TraitImplArgument) {}
    fn visit_trait_impl_argument_named(&mut self, &TraitImplArgumentNamed) {}
    fn visit_trait_impl_function(&mut self, &TraitImplFunction) {}
    fn visit_trait_impl_function_header(&mut self, &TraitImplFunctionHeader) {}
    fn visit_trait_member(&mut self, &TraitMember) {}
    fn visit_try_operator(&mut self, &TryOperator) {}
    fn visit_tuple(&mut self, &Tuple) {}
    fn visit_turbofish(&mut self, &Turbofish) {}
    fn visit_type(&mut self, &Type) {}
    fn visit_type_alias(&mut self, &TypeAlias) {}
    fn visit_type_core(&mut self, &TypeCore) {}
    fn visit_type_generics(&mut self, &TypeGenerics) {}
    fn visit_type_generics_angle(&mut self, &TypeGenericsAngle) {}
    fn visit_type_generics_function(&mut self, &TypeGenericsFunction) {}
    fn visit_type_inner(&mut self, &TypeInner) {}
    fn visit_type_reference(&mut self, &TypeReference) {}
    fn visit_unary(&mut self, &Unary) {}
    fn visit_use(&mut self, &Use) {}
    fn visit_use_tail(&mut self, &UseTail) {}
    fn visit_use_tail_glob(&mut self, &UseTailGlob) {}
    fn visit_use_tail_ident(&mut self, &UseTailIdent) {}
    fn visit_use_tail_multi(&mut self, &UseTailMulti) {}
    fn visit_value(&mut self, &Value) {}
    fn visit_visibility(&mut self, &Visibility) {}
    fn visit_where(&mut self, &Where) {}
    fn visit_whitespace(&mut self, &Whitespace) {}

    fn exit_argument(&mut self, &Argument) {}
    fn exit_array(&mut self, &Array) {}
    fn exit_attribute(&mut self, &Attribute) {}
    fn exit_binary(&mut self, &Binary) {}
    fn exit_block(&mut self, &Block) {}
    fn exit_call(&mut self, &Call) {}
    fn exit_character(&mut self, &Character) {}
    fn exit_closure(&mut self, &Closure) {}
    fn exit_closure_arg(&mut self, &ClosureArg) {}
    fn exit_comment(&mut self, &Comment) {}
    fn exit_const(&mut self, &Const) {}
    fn exit_crate(&mut self, &Crate) {}
    fn exit_dereference(&mut self, &Dereference) {}
    fn exit_enum(&mut self, &Enum) {}
    fn exit_enum_variant(&mut self, &EnumVariant) {}
    fn exit_enum_variant_body(&mut self, &EnumVariantBody) {}
    fn exit_expression(&mut self, &Expression) {}
    fn exit_field_access(&mut self, &FieldAccess) {}
    fn exit_file(&mut self, &File) {}
    fn exit_for_loop(&mut self, &ForLoop) {}
    fn exit_function(&mut self, &Function) {}
    fn exit_function_call(&mut self, &FunctionCall) {}
    fn exit_function_header(&mut self, &FunctionHeader) {}
    fn exit_generic(&mut self, &Generic) {}
    fn exit_generic_declarations(&mut self, &GenericDeclarations) {}
    fn exit_ident(&mut self, &Ident) {}
    fn exit_if(&mut self, &If) {}
    fn exit_impl(&mut self, &Impl) {}
    fn exit_impl_function(&mut self, &ImplFunction) {}
    fn exit_impl_member(&mut self, &ImplMember) {}
    fn exit_item(&mut self, &Item) {}
    fn exit_let(&mut self, &Let) {}
    fn exit_lifetime(&mut self, &Lifetime) {}
    fn exit_loop(&mut self, &Loop) {}
    fn exit_macro_call(&mut self, &MacroCall) {}
    fn exit_macro_rules(&mut self, &MacroRules) {}
    fn exit_match(&mut self, &Match) {}
    fn exit_match_arm(&mut self, &MatchArm) {}
    fn exit_method_call(&mut self, &MethodCall) {}
    fn exit_module(&mut self, &Module) {}
    fn exit_named_argument(&mut self, &NamedArgument) {}
    fn exit_number(&mut self, &Number) {}
    fn exit_pathed_ident(&mut self, &PathedIdent) {}
    fn exit_pattern(&mut self, &Pattern) {}
    fn exit_pattern_character(&mut self, &PatternCharacter) {}
    fn exit_pattern_ident(&mut self, &PatternIdent) {}
    fn exit_pattern_number(&mut self, &PatternNumber) {}
    fn exit_pattern_ref(&mut self, &PatternRef) {}
    fn exit_pattern_reference(&mut self, &PatternReference) {}
    fn exit_pattern_string(&mut self, &PatternString) {}
    fn exit_pattern_struct(&mut self, &PatternStruct) {}
    fn exit_pattern_struct_field(&mut self, &PatternStructField) {}
    fn exit_pattern_tuple(&mut self, &PatternTuple) {}
    fn exit_pattern_wildcard(&mut self, &PatternWildcard) {}
    fn exit_range(&mut self, &Range) {}
    fn exit_reference(&mut self, &Reference) {}
    fn exit_return(&mut self, &Return) {}
    fn exit_self_argument(&mut self, &SelfArgument) {}
    fn exit_slice(&mut self, &Slice) {}
    fn exit_statement(&mut self, &Statement) {}
    fn exit_string(&mut self, &String) {}
    fn exit_struct(&mut self, &Struct) {}
    fn exit_struct_definition_body(&mut self, &StructDefinitionBody) {}
    fn exit_struct_field(&mut self, &StructField) {}
    fn exit_struct_literal_field(&mut self, &StructLiteralField) {}
    fn exit_trait(&mut self, &Trait) {}
    fn exit_trait_impl_argument(&mut self, &TraitImplArgument) {}
    fn exit_trait_impl_argument_named(&mut self, &TraitImplArgumentNamed) {}
    fn exit_trait_impl_function(&mut self, &TraitImplFunction) {}
    fn exit_trait_impl_function_header(&mut self, &TraitImplFunctionHeader) {}
    fn exit_trait_member(&mut self, &TraitMember) {}
    fn exit_try_operator(&mut self, &TryOperator) {}
    fn exit_tuple(&mut self, &Tuple) {}
    fn exit_turbofish(&mut self, &Turbofish) {}
    fn exit_type(&mut self, &Type) {}
    fn exit_type_alias(&mut self, &TypeAlias) {}
    fn exit_type_core(&mut self, &TypeCore) {}
    fn exit_type_generics(&mut self, &TypeGenerics) {}
    fn exit_type_generics_angle(&mut self, &TypeGenericsAngle) {}
    fn exit_type_generics_function(&mut self, &TypeGenericsFunction) {}
    fn exit_type_inner(&mut self, &TypeInner) {}
    fn exit_type_reference(&mut self, &TypeReference) {}
    fn exit_unary(&mut self, &Unary) {}
    fn exit_use(&mut self, &Use) {}
    fn exit_use_tail(&mut self, &UseTail) {}
    fn exit_use_tail_glob(&mut self, &UseTailGlob) {}
    fn exit_use_tail_ident(&mut self, &UseTailIdent) {}
    fn exit_use_tail_multi(&mut self, &UseTailMulti) {}
    fn exit_value(&mut self, &Value) {}
    fn exit_visibility(&mut self, &Visibility) {}
    fn exit_where(&mut self, &Where) {}
    fn exit_whitespace(&mut self, &Whitespace) {}
}

// --------------------------------------------------

fn ex(start: Point, end: Point) -> Extent {
    let ex = (start.offset, end.offset);
    assert!(ex.1 >= ex.0, "{} does not come before {}", ex.1, ex.0);
    ex
}

// --------------------------------------------------

fn ext<'s, F, T>(f: F) -> impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Extent>
    where F: FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
{
    move |pm, pt| {
        let spt = pt;
        let (pt, _) = try_parse!(f(pm, pt));
        Progress::success(pt, ex(spt, pt))
    }
}

fn parse_until<'s, P>(p: P) -> impl Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, Extent>
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

fn tail<'s, F, T>(sep: &'static str, f: F) -> impl Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
    where F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
{
    move |pm, pt| {
        sequence!(pm, pt, {
            v  = f;
            _x = optional(whitespace);
            _x = optional(literal(sep));
            _x = optional(whitespace);
        }, |_, _| v)
    }
}

fn optional_whitespace<'s>(ws: Vec<Whitespace>) -> impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Vec<Whitespace>> {
    zero_or_more_append(ws, whitespace_core)
}

fn append_whitespace<'s>(ws: Vec<Whitespace>) -> impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Vec<Whitespace>> {
    one_or_more_append(ws, whitespace_core)
}

fn concat_whitespace<'s, F, T>
    (mut ws: Vec<Whitespace>, parser: F)
     -> impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, (Option<T>, Vec<Whitespace>)>
    where F: FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Option<(T, Vec<Whitespace>)>>
{
    move |pm, pt| {
        parser(pm, pt).map(|opt| {
            let val = opt.map(|(val, ws2)| {
                ws.extend(ws2);
                val
            });
            (val, ws)
        })
    }
}

// --------------------------------------------------

fn item<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Item> {
    pm.alternate(pt)
        .one(map(attribute, Item::Attribute))
        .one(map(p_const, Item::Const))
        .one(map(extern_crate, Item::ExternCrate))
        .one(map(function, Item::Function))
        .one(map(macro_rules, Item::MacroRules))
        .one(map(module, Item::Module))
        .one(map(p_enum, Item::Enum))
        .one(map(p_impl, Item::Impl))
        .one(map(p_struct, Item::Struct))
        .one(map(p_trait, Item::Trait))
        .one(map(p_use, Item::Use))
        .one(map(type_alias, Item::TypeAlias))
        .one(map(whitespace, Item::Whitespace))
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
    sequence!(pm, pt, {
        spt  = point;
        _    = literal("//");
        text = parse_until("\n");
    }, |_, pt| Comment { extent: ex(spt, pt), text })
}

fn comment_region<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Comment> {
    sequence!(pm, pt, {
        spt  = point;
        _    = literal("/*");
        text = parse_until("*/");
        _    = literal("*/");
    }, |_, pt| Comment { extent: ex(spt, pt), text })
}

fn function<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Function> {
    sequence!(pm, pt, {
        spt    = point;
        header = function_header;
        ws     = optional_whitespace(Vec::new());
        body   = block;
    }, |_, pt| Function {
        extent: ex(spt, pt),
        header,
        body,
        whitespace: ws
    })
}

fn function_header<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, FunctionHeader> {
    sequence!(pm, pt, {
        spt               = point;
        visibility        = optional(visibility);
        _                 = literal("fn");
        ws                = whitespace;
        name              = ident;
        ws                = optional_whitespace(ws);
        generics          = optional(generic_declarations);
        ws                = optional_whitespace(ws);
        arguments         = function_arglist;
        ws                = optional_whitespace(ws);
        (return_type, ws) = concat_whitespace(ws, optional(function_return_type));
        ws                = optional_whitespace(ws);
        (wheres, ws)      = concat_whitespace(ws, optional(where_clause));
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
    sequence!(pm, pt, {
        spt  = point;
        _    = literal("macro_rules!");
        ws   = append_whitespace(Vec::new());
        name = ident;
        ws   = append_whitespace(ws);
        _    = literal("{");
        body = parse_nested_until('{', '}');
        _    = literal("}");
    }, |_, pt| MacroRules {
        extent: ex(spt, pt),
        name,
        body,
        whitespace: ws,
    })
}

fn ident<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Ident> {
    let mut ci = pt.s.chars();
    let mut idx = 0;

    if let Some(c) = ci.next() {
        if UnicodeXID::is_xid_start(c) || c == '_' {
            idx += c.len_utf8();

            idx += ci.take_while(|&c| UnicodeXID::is_xid_continue(c)).map(|c| c.len_utf8()).sum();
        }
    }

    split_point_at_non_zero_offset(pt, idx, Error::IdentNotFound).map(|extent| Ident { extent })
}

fn split_point_at_non_zero_offset<'s>(pt: Point<'s>, idx: usize, e: Error) -> Progress<'s, Extent> {
    if idx == 0 {
        Progress::failure(pt, e)
    } else {
        let (_matched, tail) = pt.s.split_at(idx);
        let end = pt.offset + idx;
        let end_pt = Point { s: tail, offset: end };

        Progress::success(end_pt, (pt.offset, end))
    }
}

fn generic_declarations<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, GenericDeclarations> {
    sequence!(pm, pt, {
        spt       = point;
        _         = literal("<");
        lifetimes = zero_or_more(tail(",", lifetime));
        types     = zero_or_more(tail(",", generic_declaration));
        _         = literal(">");
    }, |_, pt| GenericDeclarations { extent: ex(spt, pt), lifetimes, types })
}

fn generic_declaration<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Generic> {
    ident(pm, pt).map(|name| Generic { extent: name.extent, name })
}

fn function_arglist<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Vec<Argument>> {
    sequence!(pm, pt, {
        _        = literal("(");
        self_arg = optional(map(self_argument, Argument::SelfArgument));
        args     = zero_or_more_append(self_arg, tail(",", function_argument));
        _        = literal(")");
    }, move |_, _| args)
}

fn self_argument<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, SelfArgument> {
    sequence!(pm, pt, {
        spt       = point;
        reference = optional(typ_ref);
        ws        = optional_whitespace(Vec::new());
        name      = ext(literal("self"));
        _         = optional(literal(","));
        ws        = optional_whitespace(ws);
    }, |_, pt| SelfArgument {
        extent: ex(spt, pt),
        reference,
        name: Ident { extent: name },
        whitespace: ws,
    })
}

fn function_argument<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Argument> {
    sequence!(pm, pt, {
        name = pattern;
        _    = literal(":");
        ws   = optional_whitespace(Vec::new());
        typ  = typ;
    }, |_, _| Argument::Named(NamedArgument { name, typ, whitespace: ws }))
}

fn function_return_type<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, (Type, Vec<Whitespace>)> {
    sequence!(pm, pt, {
        _   = literal("->");
        ws  = optional_whitespace(Vec::new());
        typ = typ;
    }, |_, _| (typ, ws))
}

fn where_clause<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, (Vec<Where>, Vec<Whitespace>)> {
    sequence!(pm, pt, {
        _  = literal("where");
        ws = whitespace;
        w  = one_or_more(tail(",", function_where));
    }, |_, _| (w, ws))
}

fn function_where<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Where> {
    sequence!(pm, pt, {
        spt    = point;
        name   = typ;
        _      = literal(":");
        ws     = optional_whitespace(Vec::new());
        bounds = one_or_more(tail("+", typ));
    }, |_, pt| Where { extent: ex(spt, pt), name, bounds, whitespace: ws })
}

fn block<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Block> {
    sequence!(pm, pt, {
        spt       = point;
        _         = literal("{");
        ws        = optional_whitespace(Vec::new());
        mut stmts = zero_or_more(statement);
        mut expr  = optional(expression);
        ws        = optional_whitespace(ws);
        _         = literal("}");
    }, |_, pt| {
        if expr.is_none() && stmts.last().map_or(false, Statement::is_implicit) {
            expr = stmts.pop().and_then(Statement::implicit);
        }

        Block {
            extent: ex(spt, pt),
            statements: stmts,
            expression: expr,
            whitespace: ws,
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
        _  = literal(";");
    }, |_, _| Statement::Explicit(expr))
}

// idea: trait w/associated types to avoid redefin fn types?

fn implicit_statement<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Statement> {
    expression_ending_in_brace(pm, pt).map(Statement::Implicit)
}

fn expression_ending_in_brace<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Expression> {
    pm.alternate(pt)
        .one(map(expr_if, Expression::If))
        .one(map(expr_for_loop, Expression::ForLoop))
        .one(map(expr_loop, Expression::Loop))
        .one(map(expr_match, Expression::Match))
        .one(map(expr_block, Expression::Block))
        .finish()
}

fn expression<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Expression> {
    let spt = pt;
    let (pt, _) = try_parse!(optional(whitespace)(pm, pt));
    let (pt, mut expression) = try_parse!({
        pm.alternate(pt)
            .one(expression_ending_in_brace)
            .one(map(expr_macro_call, Expression::MacroCall))
            .one(map(expr_let, Expression::Let))
            .one(map(expr_function_call, Expression::FunctionCall))
            .one(map(expr_tuple, Expression::Tuple))
            .one(map(expr_range, Expression::Range))
            .one(map(expr_array, Expression::Array))
            .one(map(character_literal, Expression::Character))
            .one(map(string_literal, Expression::String))
            .one(map(expr_closure, Expression::Closure))
            .one(map(expr_return, Expression::Return))
            .one(map(number_literal, Expression::Number))
            .one(map(expr_reference, Expression::Reference))
            .one(map(expr_dereference, Expression::Dereference))
            .one(map(expr_unary, Expression::Unary))
            .one(map(expr_value, Expression::Value))
            .finish()
    });

    let mut pt = pt;
    loop {
        let (pt2, tail) = try_parse!(optional(expression_tail)(pm, pt));
        pt = pt2;
        match tail {
            Some(ExpressionTail::Binary { op, rhs, whitespace }) => {
                expression = Expression::Binary(Binary {
                    extent: ex(spt, pt),
                    op,
                    lhs: Box::new(expression),
                    rhs,
                    whitespace,
                })
            }
            Some(ExpressionTail::FieldAccess { field }) => {
                expression = Expression::FieldAccess(FieldAccess {
                    extent: ex(spt, pt),
                    value: Box::new(expression),
                    field: field,
                })
            }
            Some(ExpressionTail::Call { args }) => {
                expression = Expression::Call(Call {
                    extent: ex(spt, pt),
                    target: Box::new(expression),
                    args: args
                })
            }
            Some(ExpressionTail::MethodCall { name, turbofish, args, whitespace }) => {
                expression = Expression::MethodCall(MethodCall {
                    extent: ex(spt, pt),
                    receiver: Box::new(expression),
                    name,
                    turbofish,
                    args,
                    whitespace,
                })
            }
            Some(ExpressionTail::Range { rhs }) => {
                expression = Expression::Range(Range {
                    extent: ex(spt, pt),
                    lhs: Some(Box::new(expression)),
                    rhs
                })
            }
            Some(ExpressionTail::Slice { range, whitespace }) => {
                expression = Expression::Slice(Slice {
                    extent: ex(spt, pt),
                    target: Box::new(expression),
                    range,
                    whitespace,
                })
            }
            Some(ExpressionTail::TryOperator) => {
                expression = Expression::TryOperator(TryOperator {
                    extent: ex(spt, pt),
                    target: Box::new(expression),
                })
            }
            None => break,
        }
    }

    Progress::success(pt, expression)
}

fn expr_macro_call<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, MacroCall> {
    sequence!(pm, pt, {
        spt  = point;
        name = ident;
        _    = literal("!");
        args = expr_macro_call_args;
    }, |_, pt| MacroCall { extent: ex(spt, pt), name, args })
}

fn expr_macro_call_args<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    pm.alternate(pt)
        .one(expr_macro_call_paren)
        .one(expr_macro_call_square)
        .finish()
}

fn expr_macro_call_paren<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    sequence!(pm, pt, {
        _    = literal("(");
        args = parse_nested_until('(', ')');
        _    = literal(")");
    }, |_, _| args)
}

fn expr_macro_call_square<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    sequence!(pm, pt, {
        _    = literal("[");
        args = parse_nested_until('[', ']');
        _    = literal("]");
    }, |_, _| args)
}

fn expr_let<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Let> {
    sequence!(pm, pt, {
        spt         = point;
        _           = literal("let");
        ws          = whitespace;
        pattern     = pattern;
        ws          = optional_whitespace(ws);
        (typ, ws)   = concat_whitespace(ws, optional(expr_let_type));
        ws          = optional_whitespace(ws);
        (value, ws) = concat_whitespace(ws, optional(expr_let_rhs));
    }, |_, pt| Let {
        extent: ex(spt, pt),
        pattern,
        typ,
        value: value.map(Box::new),
        whitespace: ws,
    })
}

fn expr_let_type<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, (Type, Vec<Whitespace>)> {
    sequence!(pm, pt, {
        _   = literal(":");
        ws  = optional_whitespace(Vec::new());
        typ = typ;
    }, |_, _| (typ, ws))
}

fn expr_let_rhs<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, (Expression, Vec<Whitespace>)> {
    sequence!(pm, pt, {
        _     = literal("=");
        ws    = optional_whitespace(Vec::new());
        value = expression;
    }, |_, _| (value, ws))
}

fn expr_if<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, If> {
    sequence!(pm, pt, {
        spt               = point;
        _                 = literal("if");
        ws                = whitespace;
        (condition, body) = expr_followed_by_block;
        more              = zero_or_more(expr_if_else_if);
        (else_body, ws)   = concat_whitespace(ws, optional(expr_if_else_end));
    }, move |_, pt| If {
        extent: ex(spt, pt),
        condition: Box::new(condition),
        body: Box::new(body),
        more,
        else_body: else_body.map(Box::new),
        whitespace: ws,
    })
}

fn expr_if_else_if<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, If> {
    sequence!(pm, pt, {
        _x   = optional(whitespace);
        _    = literal("else");
        _x   = optional(whitespace);
        tail = expr_if;
    }, |_, _| tail)
}

fn expr_if_else_end<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, (Block, Vec<Whitespace>)> {
    sequence!(pm, pt, {
        ws        = optional_whitespace(Vec::new());
        _         = literal("else");
        ws        = optional_whitespace(ws);
        else_body = block;
    }, |_, _| (else_body, ws))
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
    sequence!(pm, pt, {
        spt       = point;
        condition = pathed_ident;
        mpt       = point;
        ws        = optional_whitespace(Vec::new());
        body      = block;
    }, |_, _| {
        let condition = Expression::Value(Value {
            extent: ex(spt, mpt),
            name: condition,
            literal: None,
            whitespace: ws,
        });
        (condition, body)
    })
}

fn expr_for_loop<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ForLoop> {
    sequence!(pm, pt, {
        spt          = point;
        _            = literal("for");
        ws           = append_whitespace(Vec::new());
        pattern      = pattern;
        ws           = append_whitespace(ws);
        _            = literal("in");
        ws           = append_whitespace(ws);
        (iter, body) = expr_followed_by_block;
    }, |_, pt| ForLoop {
        extent: ex(spt, pt),
        pattern,
        iter: Box::new(iter),
        body: Box::new(body),
        whitespace: ws,
    })
}

fn expr_loop<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Loop> {
    sequence!(pm, pt, {
        spt  = point;
        _    = literal("loop");
        ws   = optional_whitespace(Vec::new());
        body = block;
    }, |_, pt| Loop { extent: ex(spt, pt), body: Box::new(body), whitespace: ws })
}

fn expr_match<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Match> {
    sequence!(pm, pt, {
        spt  = point;
        _    = literal("match");
        ws   = append_whitespace(Vec::new());
        head = expression;
        ws   = optional_whitespace(ws);
        _    = literal("{");
        arms = zero_or_more(match_arm);
        _    = literal("}");
    }, |_, pt| Match { extent: ex(spt, pt), head: Box::new(head), arms, whitespace: ws })
}

fn match_arm<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, MatchArm> {
    sequence!(pm, pt, {
        spt     = point;
        ws      = optional_whitespace(Vec::new());
        pattern = one_or_more(tail("|", pattern));
        ws      = optional_whitespace(ws);
        _       = literal("=>");
        ws      = optional_whitespace(ws);
        body    = expression;
        ws      = optional_whitespace(ws);
        _       = optional(literal(","));
        ws      = optional_whitespace(ws);
    }, |_, pt| MatchArm { extent: ex(spt, pt), pattern, body, whitespace: ws })
}

fn expr_tuple<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Tuple> {
    sequence!(pm, pt, {
        spt     = point;
        _       = literal("(");
        members = zero_or_more(tail(",", expression));
        _       = literal(")");
    }, |_, pt| Tuple { extent: ex(spt, pt), members })
}

fn expr_range<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Range> {
    sequence!(pm, pt, {
        spt = point;
        _   = literal("..");
        rhs = optional(expression);
    }, |_, pt| Range { extent: ex(spt, pt), lhs: None, rhs: rhs.map(Box::new) } )
}

fn expr_array<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Array> {
    sequence!(pm, pt, {
        spt     = point;
        _       = literal("[");
        members = zero_or_more(tail(",", expression));
        _       = literal("]");
    }, |_, pt| Array { extent: ex(spt, pt), members })
}

fn character_literal<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Character> {
    sequence!(pm, pt, {
        spt   = point;
        _     = literal("'");
        value = ext(char_char);
        _     = literal("'");
    }, |_, pt| Character { extent: ex(spt, pt), value })
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
        spt   = point;
        _     = literal("\"");
        value = ext(str_char);
        _     = literal("\"");
    }, |_, pt| String { extent: ex(spt, pt), value })
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
        spt   = point;
        _     = literal("r");
        h     = zero_or_more(literal("#"));
        _     = literal(r#"""#);
        value = ext(raw_raw(h.len()));
    }, |_, pt| String { extent: ex(spt, pt), value })
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
        spt  = point;
        mov  = optional(literal("move"));
        ws   = optional_whitespace(Vec::new());
        _    = literal("|");
        args = zero_or_more(tail(",", expr_closure_arg));
        _    = literal("|");
        body = expression;
    }, |_, pt| Closure {
        extent: ex(spt, pt),
        is_move: mov.is_some(),
        args,
        body: Box::new(body),
        whitespace: ws,
    })
}

fn expr_closure_arg<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ClosureArg> {
    sequence!(pm, pt, {
        name      = pattern;
        (typ, ws) = concat_whitespace(Vec::new(), optional(expr_closure_arg_type));
    }, |_, _| ClosureArg { name, typ, whitespace: ws })
}

fn expr_closure_arg_type<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, (Type, Vec<Whitespace>)> {
    sequence!(pm, pt, {
        ws  = optional_whitespace(Vec::new());
        _   = literal(":");
        ws  = optional_whitespace(ws);
        typ = typ;
    }, |_, _| (typ, ws))
}

fn expr_return<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Return> {
    sequence!(pm, pt, {
        spt   = point;
        _     = optional(literal("return"));
        ws    = append_whitespace(Vec::new());
        value = expression;
    }, |_, pt| Return {
        extent: ex(spt, pt),
        value: Box::new(value),
        whitespace: ws,
    })
}

fn expr_block<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Box<Block>> {
    block(pm, pt).map(Box::new)
}

fn number_literal<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Number> {
    pure_number(pm, pt).map(|extent| Number { extent })
}

fn pure_number<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    let idx = pt.s.chars().take_while(|&c| c.is_digit(10)).map(|c| c.len_utf8()).sum();

    split_point_at_non_zero_offset(pt, idx, Error::NumberNotFound)
}

fn expr_reference<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Reference> {
    sequence!(pm, pt, {
        spt     = point;
        _       = literal("&");
        ws      = optional_whitespace(Vec::new());
        mutable = optional(ext(literal("mut")));
        value   = expression;
    }, |_, pt| Reference {
        extent: ex(spt, pt),
        mutable,
        value: Box::new(value),
        whitespace: ws,
    } )
}

fn expr_dereference<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Dereference> {
    sequence!(pm, pt, {
        spt     = point;
        _       = literal("*");
        ws      = optional_whitespace(Vec::new());
        value   = expression;
    }, |_, pt| Dereference {
        extent: ex(spt, pt),
        value: Box::new(value),
        whitespace: ws,
    })
}

fn expr_unary<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Unary> {
    sequence!(pm, pt, {
        spt   = point;
        op    = ext(literal("!"));
        ws    = optional_whitespace(Vec::new());
        value = expression;
    }, |_, pt| Unary {
        extent: ex(spt, pt),
        op,
        value: Box::new(value),
        whitespace: ws
    })
}

fn expr_value<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Value> {
    sequence!(pm, pt, {
        spt           = point;
        name          = pathed_ident;
        ws            = optional_whitespace(Vec::new());
        (literal, ws) = concat_whitespace(ws, optional(expr_value_struct_literal));
    }, |_, pt| Value { extent: ex(spt, pt), name, literal, whitespace: ws } )
}

fn expr_value_struct_literal<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, (Vec<StructLiteralField>, Vec<Whitespace>)> {
    sequence!(pm, pt, {
        _      = literal("{");
        ws     = optional_whitespace(Vec::new());
        fields = zero_or_more(tail(",", expr_value_struct_literal_field));
        ws     = optional_whitespace(ws);
        _      = literal("}");
    }, |_, _| (fields, ws))
}

fn expr_value_struct_literal_field<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, StructLiteralField> {
    sequence!(pm, pt, {
        spt         = point;
        name        = ident;
        mpt         = point;
        ws          = optional_whitespace(Vec::new());
        (value, ws) = concat_whitespace(ws, optional(expr_value_struct_literal_field_value));
    }, |_, _| {
        let value = value.unwrap_or_else(|| Expression::Value(Value {
            extent: ex(spt, mpt),
            name: name.into(),
            literal: None,
            whitespace: ws,
        }));
        StructLiteralField { name, value }
    })
}

fn expr_value_struct_literal_field_value<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, (Expression, Vec<Whitespace>)> {
    sequence!(pm, pt, {
        _     = literal(":");
        ws    = optional_whitespace(Vec::new());
        value = expression;
    }, |_, _| (value, ws))
}

fn expr_function_call<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, FunctionCall> {
    sequence!(pm, pt, {
        spt  = point;
        name = pathed_ident;
        _    = literal("(");
        args = zero_or_more(tail(",", expression));
        _    = literal(")");
    }, |_, pt| FunctionCall { extent: ex(spt, pt), name, args })
}

fn expression_tail<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ExpressionTail> {
    pm.alternate(pt)
        .one(expr_tail_binary)
        .one(expr_tail_call)
        .one(expr_tail_method_call)
        .one(expr_tail_field_access)
        .one(expr_tail_range)
        .one(expr_tail_slice)
        .one(expr_tail_try_operator)
        .finish()
}

fn expr_tail_binary<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ExpressionTail> {
    sequence!(pm, pt, {
        ws  = optional_whitespace(Vec::new());
        op  = binary_op;
        ws  = optional_whitespace(ws);
        rhs = expression;
    }, |_, _| ExpressionTail::Binary { op, rhs: Box::new(rhs), whitespace: ws })
}

fn binary_op<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, BinaryOp> {
    // Two characters before one to avoid matching += as +
    pm.alternate(pt)
        .one(map(literal("!="), |_| BinaryOp::NotEqual))
        .one(map(literal("=="), |_| BinaryOp::Equal))
        .one(map(literal("&&"), |_| BinaryOp::BooleanAnd))
        .one(map(literal("||"), |_| BinaryOp::BooleanOr))
        .one(map(literal("+="), |_| BinaryOp::AddAssign))
        .one(map(literal("-="), |_| BinaryOp::SubAssign))
        .one(map(literal("*="), |_| BinaryOp::MulAssign))
        .one(map(literal("/="), |_| BinaryOp::DivAssign))
        .one(map(literal("%="), |_| BinaryOp::ModAssign))
        .one(map(literal("<="), |_| BinaryOp::LessThanOrEqual))
        .one(map(literal(">="), |_| BinaryOp::GreaterThanOrEqual))
        .one(map(literal("+"), |_| BinaryOp::Add))
        .one(map(literal("-"), |_| BinaryOp::Sub))
        .one(map(literal("*"), |_| BinaryOp::Mul))
        .one(map(literal("/"), |_| BinaryOp::Div))
        .one(map(literal("%"), |_| BinaryOp::Mod))
        .one(map(literal("<"), |_| BinaryOp::LessThan))
        .one(map(literal(">"), |_| BinaryOp::GreaterThan))
        .one(map(literal("="), |_| BinaryOp::Assign))
        .finish()
}

fn expr_tail_method_call<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ExpressionTail> {
    sequence!(pm, pt, {
        ws        = optional_whitespace(Vec::new());
        _         = literal(".");
        name      = ident;
        turbofish = optional(turbofish);
        _         = literal("(");
        args      = zero_or_more(tail(",", expression));
        _         = literal(")");
    }, |_, _| ExpressionTail::MethodCall { name, turbofish, args, whitespace: ws })
}

fn expr_tail_call<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ExpressionTail> {
    sequence!(pm, pt, {
        _         = literal("(");
        args      = zero_or_more(tail(",", expression));
        _         = literal(")");
    }, |_, _| ExpressionTail::Call { args })
}

fn expr_tail_field_access<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ExpressionTail> {
    sequence!(pm, pt, {
        _  = literal(".");
        field = field_name;
    }, |_, _| ExpressionTail::FieldAccess { field })
}

fn field_name<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, FieldName> {
    pm.alternate(pt)
        .one(map(ident, FieldName::Ident))
        .one(map(pure_number, FieldName::Number))
        .finish()
}

fn expr_tail_range<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ExpressionTail> {
    sequence!(pm, pt, {
        _   = literal("..");
        rhs = optional(expression);
    }, |_, _| ExpressionTail::Range { rhs: rhs.map(Box::new) })
}

fn expr_tail_slice<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ExpressionTail> {
    sequence!(pm, pt, {
        _     = literal("[");
        ws    = optional_whitespace(Vec::new());
        range = expression;
        ws    = optional_whitespace(ws);
        _     = literal("]");
    }, |_, _| ExpressionTail::Slice { range: Box::new(range), whitespace: ws })
}

fn expr_tail_try_operator<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ExpressionTail> {
    sequence!(pm, pt, {
        _     = literal("?");
    }, |_, _| ExpressionTail::TryOperator)
}

fn pathed_ident<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PathedIdent> {
    sequence!(pm, pt, {
        spt       = point;
        _         = optional(literal("::"));
        ident     = ident;
        idents    = zero_or_more_append(vec![ident], path_component);
        turbofish = optional(turbofish);
    }, |_, pt| PathedIdent { extent: ex(spt, pt), idents, turbofish })
}

fn path_component<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Ident> {
    sequence!(pm, pt, {
        _     = literal("::");
        ident = ident;
    }, |_, _| ident)
}

fn turbofish<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Turbofish> {
    sequence!(pm, pt, {
        spt   = point;
        _     = literal("::<");
        types = zero_or_more(tail(",", typ));
        _     = literal(">");
    }, |_, pt| Turbofish { extent: ex(spt, pt), types: types })
}

fn pattern<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Pattern> {
    pm.alternate(pt)
        .one(map(pattern_char, Pattern::Character))
        .one(map(pattern_number, Pattern::Number))
        .one(map(pattern_ref, Pattern::Ref))
        .one(map(pattern_reference, Pattern::Reference))
        .one(map(pattern_string, Pattern::String))
        .one(map(pattern_struct, Pattern::Struct))
        .one(map(pattern_tuple, Pattern::Tuple))
        // Must be last, otherwise it collides with struct names
        .one(map(pattern_ident, Pattern::Ident))
        .finish()
}

fn pattern_ident<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternIdent> {
    sequence!(pm, pt, {
        spt     = point;
        mutable = optional(ext(literal("mut")));
        ws      = optional_whitespace(Vec::new());
        ident   = pathed_ident;
        tuple   = optional(pattern_tuple_inner);
    }, |_, pt| PatternIdent {
        extent: ex(spt, pt),
        mutable,
        ident,
        tuple: tuple.unwrap_or_else(Vec::new),
        whitespace: ws,
    })
}

fn pattern_tuple<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternTuple> {
    sequence!(pm, pt, {
        spt = point;
        members = pattern_tuple_inner;
    }, |_, pt| PatternTuple { extent: ex(spt, pt), members })
}

fn pattern_tuple_inner<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Vec<Pattern>> {
    sequence!(pm, pt, {
        _                = literal("(");
        mut sub_patterns = zero_or_more(tail(",", pattern));
        wildcard         = optional(ext(literal("..")));
        _                = literal(")");
    }, |_, _| {
        if let Some(extent) = wildcard {
            sub_patterns.push(Pattern::Wildcard(PatternWildcard { extent }));
        }
        sub_patterns
    })
}

fn pattern_struct<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternStruct> {
    sequence!(pm, pt, {
        spt      = point;
        name     = pathed_ident;
        ws       = optional_whitespace(Vec::new());
        _        = literal("{");
        ws       = optional_whitespace(ws);
        fields   = zero_or_more(tail(",", pattern_struct_field));
        ws       = optional_whitespace(ws);
        wildcard = optional(literal(".."));
        ws       = optional_whitespace(ws);
        _        = literal("}");
    }, |_, pt| PatternStruct {
        extent: ex(spt, pt),
        name,
        fields,
        wildcard: wildcard.is_some(),
        whitespace: ws,
    })
}

fn pattern_struct_field<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternStructField> {
    sequence!(pm, pt, {
        spt           = point;
        name          = ident;
        ws            = optional_whitespace(Vec::new());
        (pattern, ws) = concat_whitespace(ws, optional(pattern_struct_field_tail));
    }, |_, pt| {
        let pattern = pattern.unwrap_or_else(|| {
            Pattern::Ident(PatternIdent {
                extent: ex(spt, pt),
                mutable: None,
                ident: name.into(),
                tuple: Vec::new(),
                whitespace: Vec::new(),
            })
        });
        PatternStructField { name, pattern, whitespace: ws }
    })
}

fn pattern_struct_field_tail<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, (Pattern, Vec<Whitespace>)> {
    sequence!(pm, pt, {
        _       = literal(":");
        ws      = optional_whitespace(Vec::new());
        pattern = pattern;
    }, |_, _| (pattern, ws))
}

fn pattern_char<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternCharacter> {
    character_literal(pm, pt).map(|value| PatternCharacter { extent: value.extent, value })
}

fn pattern_string<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternString> {
    string_literal(pm, pt).map(|value| PatternString { extent: value.extent, value })
}

fn pattern_number<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternNumber> {
    number_literal(pm, pt).map(|value| PatternNumber { extent: value.extent, value })
}

fn pattern_ref<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternRef> {
    sequence!(pm, pt, {
        spt     = point;
        _       = literal("ref");
        ws      = whitespace;
        pattern = pattern;
    }, |_, pt| PatternRef {
        extent: ex(spt, pt),
        pattern: Box::new(pattern),
        whitespace: ws
    })
}

fn pattern_reference<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternReference> {
    sequence!(pm, pt, {
        spt     = point;
        _       = literal("&");
        ws      = optional_whitespace(Vec::new());
        pattern = pattern;
    }, |_, pt| PatternReference {
        extent: ex(spt, pt),
        pattern: Box::new(pattern),
        whitespace: ws
    })
}

fn p_struct<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Struct> {
    sequence!(pm, pt, {
        spt        = point;
        visibility = optional(visibility);
        _          = literal("struct");
        ws         = whitespace;
        name       = ident;
        ws         = optional_whitespace(ws);
        generics   = optional(generic_declarations);
        ws         = optional_whitespace(ws);
        body       = struct_defn_body;
    }, |_, pt| Struct {
        extent: ex(spt, pt),
        visibility,
        name,
        generics,
        body,
        whitespace: ws,
    })
}

fn struct_defn_body<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, StructDefinitionBody> {
    sequence!(pm, pt, {
        spt    = point;
        _      = literal("{");
        ws     = optional_whitespace(Vec::new());
        fields = zero_or_more(tail(",", struct_defn_field));
        ws     = optional_whitespace(ws);
        _      = literal("}");
    }, |_, pt| StructDefinitionBody { extent: ex(spt, pt), fields, whitespace: ws })
}

fn struct_defn_field<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, StructField> {
    sequence!(pm, pt, {
        spt        = point;
        attributes = zero_or_more(struct_defn_field_attr);
        visibility = optional(visibility);
        name       = ident;
        _          = literal(":");
        ws         = optional_whitespace(Vec::new());
        typ        = typ;
    }, |_, pt| StructField {
        extent: ex(spt, pt),
        visibility,
        attributes,
        name,
        typ,
        whitespace: ws,
    })
}

fn struct_defn_field_attr<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Attribute> {
    sequence!(pm, pt, {
        attribute = attribute;
        _x = optional(whitespace);
    }, |_, _| attribute)
}

fn p_enum<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Enum> {
    sequence!(pm, pt, {
        spt        = point;
        visibility = optional(visibility);
        _          = literal("enum");
        ws         = whitespace;
        name       = ident;
        ws         = optional_whitespace(ws);
        generics   = optional(generic_declarations);
        ws         = optional_whitespace(ws);
        _          = literal("{");
        ws         = optional_whitespace(ws);
        variants   = zero_or_more(tail(",", enum_variant));
        ws         = optional_whitespace(ws);
        _          = literal("}");
    }, |_, pt| Enum {
        extent: ex(spt, pt),
        visibility,
        name,
        generics,
        variants,
        whitespace: ws,
    })
}

fn enum_variant<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, EnumVariant> {
    sequence!(pm, pt, {
        spt  = point;
        name = ident;
        ws   = optional_whitespace(Vec::new());
        body = optional(enum_variant_body);
    }, |_, pt| EnumVariant { extent: ex(spt, pt), name, body, whitespace: ws })
}

fn enum_variant_body<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, EnumVariantBody> {
    pm.alternate(pt)
        .one(map(tuple_defn_body, EnumVariantBody::Tuple))
        .one(map(struct_defn_body, EnumVariantBody::Struct))
        .finish()
}

fn p_trait<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Trait> {
    sequence!(pm, pt, {
        spt        = point;
        visibility = optional(visibility);
        _          = literal("trait");
        ws         = whitespace;
        name       = ident;
        generics   = optional(generic_declarations);
        ws         = append_whitespace(ws);
        _          = literal("{");
        ws         = optional_whitespace(ws);
        members    = zero_or_more(trait_impl_member);
        ws         = optional_whitespace(ws);
        _          = literal("}");
    }, |_, pt| Trait {
        extent: ex(spt, pt),
        visibility,
        name,
        generics,
        members,
        whitespace: ws,
    })
}

fn trait_impl_member<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TraitMember> {
    pm.alternate(pt)
        .one(map(trait_impl_function, TraitMember::Function))
        .one(map(attribute, TraitMember::Attribute))
        .one(map(whitespace, TraitMember::Whitespace))
        .finish()
}

fn trait_impl_function<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TraitImplFunction> {
    sequence!(pm, pt, {
        spt    = point;
        header = trait_impl_function_header;
        body   = trait_impl_function_body;
    }, |_, pt| TraitImplFunction { extent: ex(spt, pt), header, body })
}

fn visibility<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Visibility> {
    sequence!(pm, pt, {
        spt = point;
        _   = literal("pub");
        ws  = optional_whitespace(Vec::new());
    }, |_, pt| Visibility { extent: ex(spt, pt), whitespace: ws })
}

// TODO: Massively duplicated!!!
fn trait_impl_function_header<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TraitImplFunctionHeader> {
    sequence!(pm, pt, {
        spt               = point;
        visibility        = optional(visibility);
        _                 = literal("fn");
        ws                = optional_whitespace(Vec::new());
        name              = ident;
        generics          = optional(generic_declarations);
        arguments         = trait_impl_function_arglist;
        ws                = optional_whitespace(ws);
        (return_type, ws) = concat_whitespace(ws, optional(function_return_type));
        ws                = optional_whitespace(ws);
        (wheres, ws)      = concat_whitespace(ws, optional(where_clause));
    }, |_, pt| {
        TraitImplFunctionHeader {
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

fn trait_impl_function_arglist<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Vec<TraitImplArgument>> {
    sequence!(pm, pt, {
        _        = literal("(");
        self_arg = optional(map(self_argument, TraitImplArgument::SelfArgument));
        args     = zero_or_more_append(self_arg, tail(",", trait_impl_function_argument));
        _        = literal(")");
    }, move |_, _| args)
}

fn trait_impl_function_argument<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TraitImplArgument> {
    sequence!(pm, pt, {
        (name, ws) = concat_whitespace(Vec::new(), optional(trait_impl_function_argument_name));
        typ        = typ;
    }, |_, _| TraitImplArgument::Named(TraitImplArgumentNamed { name, typ, whitespace: ws }))
}

fn trait_impl_function_argument_name<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, (Pattern, Vec<Whitespace>)> {
    sequence!(pm, pt, {
        name = pattern;
        _    = literal(":");
        ws   = optional_whitespace(Vec::new());
    }, |_, _| (name, ws))
}

fn trait_impl_function_body<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Option<Block>> {
    pm.alternate(pt)
        .one(map(block, Some))
        .one(map(literal(";"), |_| None))
        .finish()
}

fn p_impl<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Impl> {
    sequence!(pm, pt, {
        spt              = point;
        _                = literal("impl");
        generics         = optional(generic_declarations);
        ws               = whitespace;
        (trait_name, ws) = concat_whitespace(ws, optional(p_impl_of_trait));
        type_name        = typ;
        ws               = optional_whitespace(ws);
        (wheres, ws)     = concat_whitespace(ws, optional(where_clause));
        _                = literal("{");
        ws               = optional_whitespace(ws);
        body             = zero_or_more(impl_member);
        ws               = optional_whitespace(ws);
        _                = literal("}");
    }, |_, pt| Impl {
        extent: ex(spt, pt),
        generics,
        trait_name,
        type_name,
        wheres: wheres.unwrap_or_else(Vec::new),
        body,
        whitespace: ws,
    })
}

fn p_impl_of_trait<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, (Type, Vec<Whitespace>)> {
    sequence!(pm, pt, {
        trait_name = typ;
        ws         = whitespace;
        _          = literal("for");
        ws         = append_whitespace(ws);
    }, |_, _| (trait_name, ws))
}

fn impl_member<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ImplMember> {
    pm.alternate(pt)
        .one(map(impl_function, ImplMember::Function))
        .one(map(attribute, ImplMember::Attribute))
        .one(map(whitespace, ImplMember::Whitespace))
        .finish()
}

fn impl_function<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ImplFunction> {
    sequence!(pm, pt, {
        spt    = point;
        header = function_header;
        body   = block;
    }, |_, pt| ImplFunction { extent: ex(spt, pt), header, body })
}

// TODO: optional could take E that is `into`, or just a different one

fn attribute<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Attribute> {
    sequence!(pm, pt, {
        spt           = point;
        _             = literal("#");
        is_containing = optional(ext(literal("!")));
        _             = literal("[");
        text          = parse_nested_until('[', ']');
        _             = literal("]");
    }, |_, pt| Attribute { extent: ex(spt, pt), is_containing, text })
}

fn p_const<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Const> {
    sequence!(pm, pt, {
        spt        = point;
        visibility = optional(visibility);
        _          = literal("const");
        ws         = whitespace;
        name       = ident;
        ws         = optional_whitespace(ws);
        _          = literal(":");
        ws         = optional_whitespace(ws);
        typ        = typ;
        ws         = optional_whitespace(ws);
        _          = literal("=");
        ws         = optional_whitespace(ws);
        value      = expression;
        _          = literal(";");
    }, |_, pt| Const {
        extent: ex(spt, pt),
        visibility,
        name,
        typ,
        value,
        whitespace: ws,
    })
}

fn extern_crate<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Crate> {
    sequence!(pm, pt, {
        spt  = point;
        _    = literal("extern");
        ws   = whitespace;
        _    = literal("crate");
        ws   = append_whitespace(ws);
        name = ident;
        ws   = optional_whitespace(ws);
        _    = literal(";");
    }, |_, pt| Crate { extent: ex(spt, pt), name, whitespace: ws })
}

fn p_use<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Use> {
    sequence!(pm, pt, {
        spt  = point;
        _    = literal("use");
        ws   = whitespace;
        _    = optional(literal("::"));
        path = zero_or_more(use_path_component);
        tail = use_tail;
        _    = literal(";");
    }, move |_, pt| {
        Use { extent: ex(spt, pt), path, tail, whitespace: ws }
    })
}

fn use_path_component<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Ident> {
    sequence!(pm, pt, {
        name = ident;
        _    = literal("::");
    }, |_, _| name)
}

fn use_tail<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, UseTail> {
    pm.alternate(pt)
        .one(map(use_tail_ident, UseTail::Ident))
        .one(map(use_tail_glob, UseTail::Glob))
        .one(map(use_tail_multi, UseTail::Multi))
        .finish()
}

fn use_tail_ident<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, UseTailIdent> {
    ident(pm, pt).map(|name| UseTailIdent { name })
}

fn use_tail_glob<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, UseTailGlob> {
    sequence!(pm, pt, {
        spt = point;
        _   = literal("*");
    }, |_, pt| UseTailGlob { extent: ex(spt, pt) })
}

fn use_tail_multi<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, UseTailMulti> {
    sequence!(pm, pt, {
        spt   = point;
        _     = literal("{");
        names = zero_or_more(tail(",", ident));
        _     = literal("}");
    }, |_, pt| UseTailMulti { extent: ex(spt, pt), names })
}

fn type_alias<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TypeAlias> {
    sequence!(pm, pt, {
        spt        = point;
        visibility = optional(visibility);
        _          = literal("type");
        ws         = whitespace;
        name       = typ;
        ws         = optional_whitespace(ws);
        _          = literal("=");
        ws         = optional_whitespace(ws);
        defn       = typ;
        ws         = optional_whitespace(ws);
        _          = literal(";");
    }, |_, pt| TypeAlias { extent: ex(spt, pt), visibility, name, defn, whitespace: ws })
}

fn module<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Module> {
    sequence!(pm, pt, {
        spt  = point;
        _    = literal("mod");
        ws   = whitespace;
        name = ident;
        ws   = optional_whitespace(ws);
        _    = literal("{");
        body = zero_or_more(item);
        _    = literal("}");
    }, |_, pt| Module { extent: ex(spt, pt), name, body, whitespace: ws })
}

fn typ<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Type> {
    sequence!(pm, pt, {
        spt       = point;
        reference = optional(typ_ref);
        ws        = optional_whitespace(Vec::new());
        inner     = typ_inner;
    }, |_, pt| Type { extent: ex(spt, pt), reference, inner, whitespace: ws })
}

fn typ_ref<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TypeReference> {
    sequence!(pm, pt, {
        spt      = point;
        _        = literal("&");
        ws       = optional_whitespace(Vec::new());
        lifetime = optional(lifetime);
        ws       = optional_whitespace(ws);
        mutable  = optional(ext(literal("mut")));
    }, |_, pt| TypeReference { extent: ex(spt, pt), lifetime, mutable, whitespace: ws })
}

fn typ_inner<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TypeInner> {
    pm.alternate(pt)
        .one(map(typ_core, TypeInner::Core))
        .one(map(tuple_defn_body, TypeInner::Tuple))
        .finish()
}

fn tuple_defn_body<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Vec<Type>> {
    sequence!(pm, pt, {
        _     = literal("(");
        types = zero_or_more(tail(",", typ));
        _     = literal(")");
    }, |_, _| types)
}

fn typ_core<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TypeCore> {
    sequence!(pm, pt, {
        spt           = point;
        (is_impl, ws) = concat_whitespace(Vec::new(), optional(typ_impl));
        name          = pathed_ident;
        generics      = optional(typ_generics);
    }, |_, pt| TypeCore { extent: ex(spt, pt), is_impl, name, generics, whitespace: ws })
}

fn typ_impl<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, (Extent, Vec<Whitespace>)> {
    sequence!(pm, pt, {
        spt = point;
        _   = literal("impl");
        ws  = whitespace;
    }, |_, _| (ex(spt, pt), ws))
}

fn typ_generics<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TypeGenerics> {
    pm.alternate(pt)
        .one(map(typ_generics_fn, TypeGenerics::Function))
        .one(map(typ_generics_angle, TypeGenerics::Angle))
        .finish()
}

fn typ_generics_fn<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TypeGenericsFunction> {
    sequence!(pm, pt, {
        spt               = point;
        _                 = literal("(");
        types             = zero_or_more(tail(",", typ));
        _                 = literal(")");
        ws                = optional_whitespace(Vec::new());
        (return_type, ws) = concat_whitespace(ws, optional(function_return_type));
    }, |_, pt| TypeGenericsFunction {
        extent: ex(spt, pt),
        types,
        return_type: return_type.map(Box::new),
        whitespace: ws,
    })
}

fn typ_generics_angle<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TypeGenericsAngle> {
    sequence!(pm, pt, {
        spt       = point;
        _         = literal("<");
        ws        = optional_whitespace(Vec::new());
        lifetimes = zero_or_more(tail(",", lifetime));
        types     = zero_or_more(tail(",", typ));
        _         = literal(">");
    }, |_, pt| TypeGenericsAngle { extent: ex(spt, pt), lifetimes, types, whitespace: ws })
}

fn lifetime<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Lifetime> {
    sequence!(pm, pt, {
        spt  = point;
        _    = literal("'");
        ws   = optional_whitespace(Vec::new());
        name = ident;
    }, |_, pt| Lifetime { extent: ex(spt, pt), name, whitespace: ws })
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
    fn parse_use() {
        let p = qp(p_use, "use foo::Bar;");
        assert_eq!(unwrap_progress(p).extent, (0, 13))
    }

    #[test]
    fn parse_use_glob() {
        let p = qp(p_use, "use foo::*;");
        assert_eq!(unwrap_progress(p).extent, (0, 11))
    }

    #[test]
    fn parse_use_with_multi() {
        let p = qp(p_use, "use foo::{Bar, Baz};");
        assert_eq!(unwrap_progress(p).extent, (0, 20))
    }

    #[test]
    fn parse_use_no_path() {
        let p = qp(p_use, "use {Bar, Baz};");
        assert_eq!(unwrap_progress(p).extent, (0, 15))
    }

    #[test]
    fn parse_use_absolute_path() {
        let p = qp(p_use, "use ::{Bar, Baz};");
        assert_eq!(unwrap_progress(p).extent, (0, 17))
    }

    #[test]
    fn item_mod_multiple() {
        let p = qp(item, "mod foo { use super::*; }");
        assert_eq!(unwrap_progress(p).extent(), (0, 25))
    }

    #[test]
    fn item_macro_rules() {
        let p = qp(macro_rules, "macro_rules! foo { }");
        assert_eq!(unwrap_progress(p).extent, (0, 20))
    }

    #[test]
    fn item_mod() {
        let p = qp(module, "mod foo { }");
        assert_eq!(unwrap_progress(p).extent, (0, 11))
    }

    #[test]
    fn item_trait() {
        let p = qp(item, "trait Foo {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 12))
    }

    #[test]
    fn item_trait_public() {
        let p = qp(item, "pub trait Foo {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 16))
    }

    #[test]
    fn item_trait_with_generics() {
        let p = qp(item, "trait Foo<T> {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 15))
    }

    #[test]
    fn item_trait_with_members() {
        let p = qp(item, "trait Foo { fn bar(&self) -> u8; }");
        assert_eq!(unwrap_progress(p).extent(), (0, 34))
    }

    #[test]
    fn item_trait_with_members_with_patterns() {
        let p = qp(item, "trait Foo { fn bar(&self, &a: &u8) -> u8; }");
        assert_eq!(unwrap_progress(p).extent(), (0, 43))
    }

    #[test]
    fn item_trait_with_members_with_body() {
        let p = qp(item, "trait Foo { fn bar(&self) -> u8 { 42 } }");
        assert_eq!(unwrap_progress(p).extent(), (0, 40))
    }

    #[test]
    fn item_trait_with_unnamed_parameters() {
        let p = qp(item, "trait Foo { fn bar(&self, u8); }");
        assert_eq!(unwrap_progress(p).extent(), (0, 32))
    }

    #[test]
    fn item_type_alias() {
        let p = qp(item, "type Foo<T> = Bar<T, u8>;");
        assert_eq!(unwrap_progress(p).extent(), (0, 25))
    }

    #[test]
    fn item_type_alias_public() {
        let p = qp(item, "pub type Foo<T> = Bar<T, u8>;");
        assert_eq!(unwrap_progress(p).extent(), (0, 29))
    }

    #[test]
    fn item_const() {
        let p = qp(item, r#"const FOO: &'static str = "hi";"#);
        assert_eq!(unwrap_progress(p).extent(), (0, 31))
    }

    #[test]
    fn item_const_public() {
        let p = qp(item, "pub const FOO: u8 = 42;");
        assert_eq!(unwrap_progress(p).extent(), (0, 23))
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
    fn impl_with_trait_bounds() {
        let p = qp(p_impl, "impl<T> Foo for Bar<T> where T: Quux {}");
        assert_eq!(unwrap_progress(p).extent, (0, 39))
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
    fn enum_with_generic_declarations() {
        let p = qp(p_enum, "enum A<T> { Foo(Vec<T>) }");
        assert_eq!(unwrap_progress(p).extent, (0, 25))
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
    fn fn_with_self_type_with_lifetime() {
        let p = qp(function_header, "fn foo<'a>(&'a self)");
        assert_eq!(unwrap_progress(p).extent, (0, 20))
    }

    #[test]
    fn fn_with_self_type_and_regular() {
        let p = qp(function_header, "fn foo(&self, a: u8)");
        assert_eq!(unwrap_progress(p).extent, (0, 20))
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
    fn fn_with_arguments_with_patterns() {
        let p = qp(function_header, "fn foo(&a: &u8)");
        assert_eq!(unwrap_progress(p).extent, (0, 15))
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
    fn fn_with_whitespace_before_arguments() {
        let p = qp(function_header, "fn foo () -> ()");
        assert_eq!(unwrap_progress(p).extent, (0, 15))
    }

    #[test]
    fn fn_with_whitespace_before_generics() {
        let p = qp(function_header, "fn foo <'a, T>() -> ()");
        assert_eq!(unwrap_progress(p).extent, (0, 22))
    }

    #[test]
    fn block_promotes_implicit_statement_to_expression() {
        let p = qp(block, "{ if a {} }");
        let p = unwrap_progress(p);
        assert!(p.statements.is_empty());
        assert_eq!(p.expression.unwrap().extent(), (2, 9));
    }

    #[test]
    fn statement_match_no_semicolon() {
        let p = qp(statement, "match a { _ => () }");
        assert_eq!(unwrap_progress(p).implicit().unwrap().extent(), (0, 19))
    }

    #[test]
    fn statement_use() {
        let p = qp(statement, "use foo::Bar;");
        assert_eq!(unwrap_progress(p).extent(), (0, 13))
    }

    #[test]
    fn expr_true() {
        let p = qp(expression, "true");
        assert_eq!(unwrap_progress(p).extent(), (0, 4))
    }

    #[test]
    fn expr_number() {
        let p = qp(expression, "123");
        assert_eq!(unwrap_progress(p).extent(), (0, 3))
    }

    #[test]
    fn expr_let_explicit_type() {
        let p = qp(expression, "let foo: bool");
        assert_eq!(unwrap_progress(p).extent(), (0, 13))
    }

    #[test]
    fn expr_let_mut() {
        let p = qp(expression, "let mut pm = Master::new()");
        assert_eq!(unwrap_progress(p).extent(), (0, 26))
    }

    #[test]
    fn expr_let_no_value() {
        let p = qp(expression, "let pm");
        assert_eq!(unwrap_progress(p).extent(), (0, 6))
    }

    #[test]
    fn expr_assign() {
        let p = qp(expression, "a = b");
        assert_eq!(unwrap_progress(p).extent(), (0, 5))
    }

    #[test]
    fn expr_assign_to_field() {
        let p = qp(expression, "a.b = c");
        assert_eq!(unwrap_progress(p).extent(), (0, 7))
    }

    #[test]
    fn expr_value_with_path() {
        let p = qp(expression, "Master::new()");
        assert_eq!(unwrap_progress(p).extent(), (0, 13))
    }

    #[test]
    fn expr_field_access_name() {
        let p = qp(expression, "foo.bar");
        assert_eq!(unwrap_progress(p).extent(), (0, 7))
    }

    #[test]
    fn expr_field_access_number() {
        let p = qp(expression, "foo.0");
        assert_eq!(unwrap_progress(p).extent(), (0, 5))
    }

    #[test]
    fn expr_field_access_multiple() {
        let p = qp(expression, "foo.bar.baz");
        assert_eq!(unwrap_progress(p).extent(), (0, 11))
    }

    #[test]
    fn expr_function_call() {
        let p = qp(expression, "foo()");
        assert_eq!(unwrap_progress(p).extent(), (0, 5))
    }

    #[test]
    fn pathed_ident_with_leading_separator() {
        let p = qp(pathed_ident, "::foo");
        assert_eq!(unwrap_progress(p).extent, (0, 5))
    }

    #[test]
    fn pathed_ident_with_turbofish() {
        let p = qp(pathed_ident, "foo::<Vec<u8>>");
        assert_eq!(unwrap_progress(p).extent, (0, 14))
    }

    #[test]
    fn expr_function_call_with_args() {
        let p = qp(expression, "foo(true)");
        assert_eq!(unwrap_progress(p).extent(), (0, 9))
    }

    #[test]
    fn expr_method_call() {
        let p = qp(expression, "foo.bar()");
        assert_eq!(unwrap_progress(p).extent(), (0, 9))
    }

    #[test]
    fn expr_method_call_multiple() {
        let p = qp(expression, "foo.bar().baz()");
        assert_eq!(unwrap_progress(p).extent(), (0, 15))
    }

    #[test]
    fn expr_method_call_multiple_spaced() {
        let p = qp(expression, "foo.bar()\n    .baz()");
        assert_eq!(unwrap_progress(p).extent(), (0, 20))
    }

    #[test]
    fn expr_method_call_with_turbofish() {
        let p = qp(expression, "foo.bar::<u8>()");
        assert_eq!(unwrap_progress(p).extent(), (0, 15))
    }

    #[test]
    fn expr_method_call_with_turbofish_nested() {
        let p = qp(expression, "e.into_iter().collect::<BTreeSet<_>>()");
        assert_eq!(unwrap_progress(p).extent(), (0, 38))
    }

    #[test]
    fn expr_call_of_expr() {
        let p = qp(expression, "{foo}()");
        assert_eq!(unwrap_progress(p).extent(), (0, 7))
    }

    #[test]
    fn expr_for_loop() {
        let p = qp(expression, "for (a, b) in c {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 18))
    }

    #[test]
    fn expr_loop() {
        let p = qp(expression, "loop {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 7))
    }

    #[test]
    fn expr_match() {
        let p = qp(expression, "match foo { _ => () }");
        assert_eq!(unwrap_progress(p).extent(), (0, 21))
    }

    #[test]
    fn expr_tuple() {
        let p = qp(expression, "(1, 2)");
        assert_eq!(unwrap_progress(p).extent(), (0, 6))
    }

    #[test]
    fn expr_block() {
        let p = qp(expression, "{}");
        assert_eq!(unwrap_progress(p).extent(), (0, 2))
    }

    #[test]
    fn expr_if_() {
        let p = qp(expression, "if a {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 7))
    }

    #[test]
    fn expr_if_else() {
        let p = qp(expression, "if a {} else {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 15))
    }

    #[test]
    fn expr_if_else_if() {
        let p = qp(expression, "if a {} else if b {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 20))
    }

    #[test]
    fn expr_binary_op() {
        let p = qp(expression, "a < b");
        assert_eq!(unwrap_progress(p).extent(), (0, 5))
    }

    #[test]
    fn expr_binary_multiple() {
        let p = qp(expression, "1 + 2 + 3");
        assert_eq!(unwrap_progress(p).extent(), (0, 9))
    }

    #[test]
    fn expr_binary_op_two_char() {
        let p = qp(expression, "a >= b");
        assert_eq!(unwrap_progress(p).extent(), (0, 6))
    }

    #[test]
    fn expr_binary_op_equality() {
        let p = qp(expression, "a == b != c");
        assert_eq!(unwrap_progress(p).extent(), (0, 11))
    }

    #[test]
    fn expr_binary_op_boolean_logic() {
        let p = qp(expression, "a && b || c");
        assert_eq!(unwrap_progress(p).extent(), (0, 11))
    }

    #[test]
    fn expr_braced_true() {
        let p = qp(expression, "{ true }");
        assert_eq!(unwrap_progress(p).extent(), (0, 8))
    }

    #[test]
    fn expr_macro_call_with_nested_parens() {
        let p = qp(expression, "foo!(())");
        assert_eq!(unwrap_progress(p).extent(), (0, 8))
    }

    #[test]
    fn expr_macro_call_with_square_brackets() {
        let p = qp(expression, "vec![]");
        assert_eq!(unwrap_progress(p).extent(), (0, 6))
    }

    #[test]
    fn expr_range_both() {
        let p = qp(expression, "1..2");
        assert_eq!(unwrap_progress(p).extent(), (0, 4))
    }

    #[test]
    fn expr_range_left() {
        let p = qp(expression, "3..");
        assert_eq!(unwrap_progress(p).extent(), (0, 3))
    }

    #[test]
    fn expr_range_right() {
        let p = qp(expression, "..4");
        assert_eq!(unwrap_progress(p).extent(), (0, 3))
    }

    #[test]
    fn expr_range_none() {
        let p = qp(expression, "..");
        assert_eq!(unwrap_progress(p).extent(), (0, 2))
    }

    #[test]
    fn expr_value_struct_literal() {
        let p = qp(expression, "Point { a: 1 }");
        assert_eq!(unwrap_progress(p).extent(), (0, 14))
    }

    #[test]
    fn expr_value_struct_literal_shortahnd() {
        let p = qp(expression, "Point { a }");
        assert_eq!(unwrap_progress(p).extent(), (0, 11))
    }

    #[test]
    fn expr_closure() {
        let p = qp(expression, "|a| a");
        assert_eq!(unwrap_progress(p).extent(), (0, 5))
    }

    #[test]
    fn expr_closure_multiple() {
        let p = qp(expression, "|a, b| a + b");
        assert_eq!(unwrap_progress(p).extent(), (0, 12))
    }

    #[test]
    fn expr_closure_explicit_type() {
        let p = qp(expression, "|a: u8| a");
        assert_eq!(unwrap_progress(p).extent(), (0, 9))
    }

    #[test]
    fn expr_closure_pattern() {
        let p = qp(expression, "|&a| a");
        assert_eq!(unwrap_progress(p).extent(), (0, 6))
    }

    #[test]
    fn expr_closure_move() {
        let p = qp(expression, "move || 42");
        assert_eq!(unwrap_progress(p).extent(), (0, 10))
    }

    #[test]
    fn expr_return() {
        let p = qp(expression, "return 1");
        assert_eq!(unwrap_progress(p).extent(), (0, 8))
    }

    #[test]
    fn expr_array() {
        let p = qp(expression, "[1, 1]");
        assert_eq!(unwrap_progress(p).extent(), (0, 6))
    }

    #[test]
    fn expr_char_literal() {
        let p = qp(expression, "'a'");
        assert_eq!(unwrap_progress(p).extent(), (0, 3))
    }

    #[test]
    fn expr_char_literal_escape() {
        let p = qp(expression, r"'\''");
        assert_eq!(unwrap_progress(p).extent(), (0, 4))
    }

    #[test]
    fn expr_string_literal() {
        let p = qp(expression, r#""a""#);
        assert_eq!(unwrap_progress(p).extent(), (0, 3))
    }

    #[test]
    fn expr_string_literal_escape() {
        let p = qp(expression, r#""\"""#);
        assert_eq!(unwrap_progress(p).extent(), (0, 4))
    }

    #[test]
    fn expr_string_literal_raw() {
        let p = qp(expression, r###"r#"foo"#"###);
        assert_eq!(unwrap_progress(p).extent(), (0, 8))
    }

    #[test]
    fn expr_slice_index() {
        let p = qp(expression, "a[..2]");
        assert_eq!(unwrap_progress(p).extent(), (0, 6))
    }

    #[test]
    fn expr_reference() {
        let p = qp(expression, "&foo");
        assert_eq!(unwrap_progress(p).extent(), (0, 4))
    }

    #[test]
    fn expr_reference_mut() {
        let p = qp(expression, "&mut foo");
        assert_eq!(unwrap_progress(p).extent(), (0, 8))
    }

    #[test]
    fn expr_dereference() {
        let p = qp(expression, "*foo");
        assert_eq!(unwrap_progress(p).extent(), (0, 4))
    }

    #[test]
    fn expr_not() {
        let p = qp(expression, "!foo");
        assert_eq!(unwrap_progress(p).extent(), (0, 4))
    }

    #[test]
    fn expr_try_operator() {
        let p = qp(expression, "foo?");
        assert_eq!(unwrap_progress(p).extent(), (0, 4))
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
    fn pattern_with_string_literal() {
        let p = qp(pattern, r#""hello""#);
        assert_eq!(unwrap_progress(p).extent(), (0, 7))
    }

    #[test]
    fn pattern_with_numeric_literal() {
        let p = qp(pattern, "42");
        assert_eq!(unwrap_progress(p).extent(), (0, 2))
    }

    #[test]
    fn pattern_with_reference() {
        let p = qp(pattern, "&a");
        assert_eq!(unwrap_progress(p).extent(), (0, 2))
    }

    #[test]
    fn pattern_with_ref() {
        let p = qp(pattern, "ref a");
        assert_eq!(unwrap_progress(p).extent(), (0, 5))
    }

    #[test]
    fn match_arm_with_alternate() {
        let p = qp(match_arm, "a | b => 1");
        assert_eq!(unwrap_progress(p).extent, (0, 10))
    }

    #[test]
    fn type_tuple() {
        let p = qp(typ, "(u8, u8)");
        assert_eq!(unwrap_progress(p).extent, (0, 8))
    }

    #[test]
    fn type_with_generics() {
        let p = qp(typ, "A<T>");
        assert_eq!(unwrap_progress(p).extent, (0, 4))
    }

    #[test]
    fn type_impl_trait() {
        let p = qp(typ, "impl Foo");
        assert_eq!(unwrap_progress(p).extent, (0, 8))
    }

    #[test]
    fn type_fn_trait() {
        let p = qp(typ, "Fn(u8) -> u8");
        assert_eq!(unwrap_progress(p).extent, (0, 12))
    }

    #[test]
    fn type_mut_ref() {
        let p = qp(typ, "&mut Foo");
        assert_eq!(unwrap_progress(p).extent, (0, 8))
    }

    #[test]
    fn type_mut_ref_with_lifetime() {
        let p = qp(typ, "&'a mut Foo");
        assert_eq!(unwrap_progress(p).extent, (0, 11))
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
    fn struct_with_generic_declarations() {
        let p = qp(p_struct, "struct S<T> { field: Option<T> }");
        assert_eq!(unwrap_progress(p).extent, (0, 32))
    }

    #[test]
    fn struct_public() {
        let p = qp(p_struct, "pub struct S {}");
        assert_eq!(unwrap_progress(p).extent, (0, 15))
    }

    #[test]
    fn struct_public_field() {
        let p = qp(p_struct, "struct S { pub age: u8 }");
        assert_eq!(unwrap_progress(p).extent, (0, 24))
    }

    #[test]
    fn struct_with_attributed_field() {
        let p = qp(p_struct, "struct S { #[foo(bar)] #[baz(quux)] field: u8 }");
        assert_eq!(unwrap_progress(p).extent, (0, 47))
    }

    #[test]
    fn where_clause_with_path() {
        let p = qp(function_where, "P: foo::bar::baz::Quux<'a>");
        assert_eq!(unwrap_progress(p).extent, (0, 26))
    }

    #[test]
    fn where_clause_with_multiple_bounds() {
        let p = qp(function_where, "P: A + B");
        assert_eq!(unwrap_progress(p).extent, (0, 8))
    }

    #[test]
    fn where_clause_with_multiple_types() {
        let p = qp(where_clause, "where P: A, Q: B");
        let (p, _) = unwrap_progress(p);
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

    #[test]
    fn ident_with_leading_underscore() {
        let p = qp(ident, "_foo");
        assert_eq!(unwrap_progress(p).extent, (0, 4))
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
