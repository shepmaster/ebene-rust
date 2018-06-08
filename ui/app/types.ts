export enum Kind {
    Nothing = 'Nothing',
    Layer = 'Layer',
    Term = 'Term',
    Containing = 'Containing',
    ContainedIn = 'ContainedIn',
    NotContaining = 'NotContaining',
    NotContainedIn = 'NotContainedIn',
    OneOf = 'OneOf',
    BothOf = 'BothOf',
    FollowedBy = 'FollowedBy',
}

export type BinaryKind =
    | Kind.BothOf
    | Kind.ContainedIn
    | Kind.Containing
    | Kind.FollowedBy
    | Kind.NotContainedIn
    | Kind.NotContaining
    | Kind.OneOf
    ;

export const makeNothing: () => FlatNothing = () => ({ kind: Kind.Nothing });

export type FlatQueryItem =
    | FlatTerm
    | FlatBinary<Kind.Containing>
    | FlatBinary<Kind.ContainedIn>
    | FlatBinary<Kind.NotContaining>
    | FlatBinary<Kind.NotContainedIn>
    | FlatBinary<Kind.OneOf>
    | FlatBinary<Kind.BothOf>
    | FlatBinary<Kind.FollowedBy>
    | FlatLayer
    | FlatNothing;

export interface FlatTerm {
    kind: Kind.Term,
    name: string,
    value: string,
}

export interface FlatBinary<T extends Kind> {
    kind: T,
    lhs: number,
    rhs: number,
}

export interface FlatLayer {
    kind: Kind.Layer,
    name: string,
}

export interface FlatNothing {
    kind: Kind.Nothing,
}

export interface FlatQueryItems {
    [index: number]: FlatQueryItem;
}

export interface QueryResult { }
