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
