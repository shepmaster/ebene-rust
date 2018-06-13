import { Kind, QueryResult } from './types';

export enum ActionType {
    AdvancedHighlightUpdate = 'ADVANCED_HIGHLIGHT_UPDATE',
    AvailableLayersUpdate = 'AVAILABLE_LAYERS_UPDATE',
    AvailableTermsUpdate = 'AVAILABLE_TERMS_UPDATE',
    AdvancedQueryUpdate = 'ADVANCED_QUERY_UPDATE',
    HighlightAdd = 'HIGHLIGHT_ADD',
    LayerNameUpdate = 'LAYER_NAME_UPDATE',
    QueryResults = 'QUERY_RESULTS',
    QueryToggle = 'QUERY_TOGGLE',
    StructuredQueryKindUpdate = 'STRUCTURED_QUERY_KIND_UPDATE',
    TermNameUpdate = 'TERM_NAME_UPDATE',
    TermValueUpdate = 'TERM_VALUE_UPDATE',
}

// The general shape of any possible Redux action. More complicated
// than the upstream version to allow a specific type for the type
// property.
export interface FluxStandardAction<T extends string, Payload, Meta = undefined> {
    type: T;
    payload?: Payload;
    error?: boolean;
    meta?: Meta;
}

// Common variants of the action
interface SuccessAction<T extends ActionType> extends FluxStandardAction<T, undefined> {
    error: undefined;
}
interface SuccessActionPayload<T extends ActionType, P> extends FluxStandardAction<T, P> {
    payload: P;
    error: undefined;
}
interface ErrorActionPayload<T extends ActionType, P extends Error> extends FluxStandardAction<T, P> {
    payload: P;
    error: true;
}

function createAction<T extends ActionType>(type: T): SuccessAction<T>
function createAction<T extends ActionType, P>(type: T, payload: P): SuccessActionPayload<T, P>
function createAction<T extends ActionType, P>(type: T, payload?: P) {
    return payload === undefined ? { type } : { type, payload };
}

function createActionError<T extends ActionType, P extends Error>(type: T, payload: P): ErrorActionPayload<T, P> {
    return { type, payload, error: true };
}

export const toggleAdvanced = () =>
    createAction(ActionType.QueryToggle);

export const updateAdvancedQuery = (query: string) =>
    createAction(ActionType.AdvancedQueryUpdate, { query });

export const updateAdvancedHighlight = (highlight: string) =>
    createAction(ActionType.AdvancedHighlightUpdate, { highlight });

export const updateQueryResults = (results: QueryResult[]) =>
    createAction(ActionType.QueryResults, { results });

export const queryFailed = (message: string) =>
    createActionError(ActionType.QueryResults, new Error(message));

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

export const updateAvailableLayers = (layers: string[]) =>
    createAction(ActionType.AvailableLayersUpdate, { layers });

export const updateAvailableTerms = (terms: string[]) =>
    createAction(ActionType.AvailableTermsUpdate, { terms });

export type Action =
    | ReturnType<typeof highlightAdd>
    | ReturnType<typeof queryFailed>
    | ReturnType<typeof toggleAdvanced>
    | ReturnType<typeof updateAdvancedHighlight>
    | ReturnType<typeof updateAdvancedQuery>
    | ReturnType<typeof updateAvailableLayers>
    | ReturnType<typeof updateAvailableTerms>
    | ReturnType<typeof updateKind>
    | ReturnType<typeof updateLayerName>
    | ReturnType<typeof updateQueryResults>
    | ReturnType<typeof updateTermName>
    | ReturnType<typeof updateTermValue>
    ;

// Higher order action creator modifiers

export interface WithTarget {
    meta: {
        target: string;
    }
}

export function withTarget<T extends string, M extends {}, A extends FluxStandardAction<T, M>>(action: (...args: any[]) => A, target: string) {
    return function(...args: any[]): A & WithTarget {
        let act = action(...args);
        let meta = Object.assign({}, act.meta, { target });
        return Object.assign(act, { meta });
    }
}

export function isWithTarget(a: any): a is WithTarget {
    return !!a['meta'] && typeof a['meta']['target'] == "string";
}

export interface WithTargetIndex {
    meta: {
        targetIndex: number;
    }
}

export function withTargetIndex<T extends string, M extends {}, A extends FluxStandardAction<T, M>>(action: (...args: any[]) => A, index: number) {
    return function(...args: any[]): A & WithTargetIndex {
        let act = action(...args);
        let meta = Object.assign({}, act.meta, { targetIndex: index });
        return Object.assign(act, { meta });
    }
}

export function isWithTargetIndex(a: any): a is WithTargetIndex {
    return !!a['meta'] && typeof a['meta']['targetIndex'] == "number";
}
