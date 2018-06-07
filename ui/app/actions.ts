export enum ActionType {
    QueryToggle = 'QUERY_TOGGLE',
    AdvancedQueryUpdate = 'ADVANCED_QUERY_UPDATE',
    AdvancedHighlightUpdate = 'ADVANCED_HIGHLIGHT_UPDATE',
    QueryResults = 'QUERY_RESULTS',
    StructuredQueryKindUpdate = 'STRUCTURED_QUERY_KIND_UPDATE',
    LayerNameUpdate = 'LAYER_NAME_UPDATE',
    TermNameUpdate = 'TERM_NAME_UPDATE',
    TermValueUpdate = 'TERM_VALUE_UPDATE',
    HighlightAdd = 'HIGHLIGHT_ADD',
}

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

interface SuccessAction<T extends string> { type: T, error: undefined }
interface SuccessActionPayload<T extends string, P> { type: T, payload: P, error: undefined }
interface ErrorAction<T extends string> { type: T, error: true }
interface ErrorActionPayload<T extends string, P> { type: T, payload: P, error: true }

function createAction<T extends string, P>(type: T): SuccessAction<T>
function createAction<T extends string, P>(type: T, payload: P): SuccessActionPayload<T, P>
function createAction<T extends string, P>(type: T, payload?: P) {
    return payload === undefined ? { type } : { type, payload };
}

function createActionError<T extends string, P>(type: T): ErrorAction<T>
function createActionError<T extends string, P>(type: T, payload: P): ErrorActionPayload<T, P>
function createActionError<T extends string, P>(type: T, payload?: P) {
    return payload === undefined ? { type, error: true } : { type, payload, error: true };
}

export const toggleAdvanced = () =>
    createAction(ActionType.QueryToggle);

export const updateAdvancedQuery = (query: string) =>
    createAction(ActionType.AdvancedQueryUpdate, { query });

export const updateAdvancedHighlight = (highlight: string) =>
    createAction(ActionType.AdvancedHighlightUpdate, { highlight });

export const updateQueryResults = (results) =>
    createAction(ActionType.QueryResults, { results });

export const queryFailed = (message: string) =>
    createActionError(ActionType.QueryResults, { message });

export const updateKind = (id: number, kind: Kind) =>
    createAction(ActionType.StructuredQueryKindUpdate, { id, kind });

export const updateLayerName = (id: number, name: string) =>
    createAction(ActionType.LayerNameUpdate, { id, name });

export const updateTermName = (id: number, name: string) =>
    createAction(ActionType.TermNameUpdate, { id, name });

export const updateTermValue = (id: number, value: string) =>
    createAction(ActionType.TermValueUpdate, { id, value });

export const highlightAdd = (index: number) =>
    createAction(ActionType.HighlightAdd, { index });

export type Action =
    | ReturnType<typeof toggleAdvanced>
    | ReturnType<typeof updateAdvancedQuery>
    | ReturnType<typeof updateAdvancedHighlight>
    | ReturnType<typeof updateQueryResults>
    | ReturnType<typeof queryFailed>
    | ReturnType<typeof updateKind>
    | ReturnType<typeof updateLayerName>
    | ReturnType<typeof updateTermName>
    | ReturnType<typeof updateTermValue>
    | ReturnType<typeof highlightAdd>
    ;

export const retarget = (action, target: string) => (...args) => {
    let createdAction = action(...args);
    createdAction.payload.target = target;
    return createdAction;
};

export const retargetIndex = (action, index: number) => (...args) => {
    let createdAction = action(...args);
    createdAction.payload.targetIndex = index;
    return createdAction;
};
