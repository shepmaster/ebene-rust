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

export function isBinaryKind(kind: Kind): kind is BinaryKind {
    switch (kind) {
        case Kind.Containing:
        case Kind.ContainedIn:
        case Kind.NotContaining:
        case Kind.NotContainedIn:
        case Kind.OneOf:
        case Kind.BothOf:
        case Kind.FollowedBy:
            return true;
        default:
            return false;
    }
}

// The flat query is stored in the state and is a flat mapping of ID number to data

export type FlatQueryItem =
    | FlatTerm
    | FlatBinaryItem
    | FlatLayer
    | FlatNothing
    ;

export type FlatBinaryItem =
    | FlatBinary<Kind.Containing>
    | FlatBinary<Kind.ContainedIn>
    | FlatBinary<Kind.NotContaining>
    | FlatBinary<Kind.NotContainedIn>
    | FlatBinary<Kind.OneOf>
    | FlatBinary<Kind.BothOf>
    | FlatBinary<Kind.FollowedBy>
    ;

export interface FlatBinary<T extends Kind> {
    kind: T,
    lhs: number,
    rhs: number,
}

export function isFlatBinaryItem(item: FlatQueryItem): item is FlatBinaryItem {
    return isBinaryKind(item.kind);
}

export interface FlatTerm {
    kind: Kind.Term,
    name: string,
    value: string,
}

export interface FlatLayer {
    kind: Kind.Layer,
    name: string,
}

export interface FlatNothing {
    kind: Kind.Nothing,
}

export const makeNothing: () => FlatNothing = () => ({ kind: Kind.Nothing });

export interface FlatQueryItems {
    [index: number]: FlatQueryItem;
}

// The tree query is used by the UI and reflects the recursive structure

export type TreeQueryItem =
    | TreeTerm
    | TreeBinary<Kind.Containing>
    | TreeBinary<Kind.ContainedIn>
    | TreeBinary<Kind.NotContaining>
    | TreeBinary<Kind.NotContainedIn>
    | TreeBinary<Kind.OneOf>
    | TreeBinary<Kind.BothOf>
    | TreeBinary<Kind.FollowedBy>
    | TreeLayer
    | TreeNothing;

export interface TreeTerm {
    id: number,
    kind: Kind.Term,
    name: string,
    value: string,
}

export interface TreeBinary<T extends Kind> {
    id: number,
    kind: T,
    lhs: TreeQueryItem,
    rhs: TreeQueryItem,
}

export interface TreeLayer {
    id: number,
    kind: Kind.Layer,
    name: string,
}

export interface TreeNothing {
    id: number,
    kind: Kind.Nothing,
}

export type Extents = [number, number][];

export interface QueryResult {
    text: string;
    highlights: Extents[];
}
