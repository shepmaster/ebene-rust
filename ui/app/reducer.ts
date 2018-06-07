import { combineReducers } from 'redux';

import { ActionType, Action, Kind } from './actions';

// --- Higher order reducers

const initial = (reducer, initialState) => (state = initialState, action) => (
    reducer(state, action)
);

const forTarget = (reducer, expectedTarget: string) => (state, action) => {
    if (action.payload && action.payload.target === expectedTarget) {
        return reducer(state, action);
    } else {
        return state;
    }
};

const forTargetIndex = (reducer) => (state, action) => {
    const index = action.payload.targetIndex;
    const result = reducer(state[index], action);
    const newState = state.slice();
    newState.splice(index, 1, result);
    return newState;
};

// --- Regular reducers

const initialAdvState = {
    query: '{"Layer": {"name": "function"}}',
    highlight: '[{"Term": {"name": "ident", "value": "pm"}}]',
};

function advancedReducer(state = initialAdvState, action: Action) {
    switch (action.type) {
        case ActionType.AdvancedQueryUpdate: {
            const { query } = action.payload;
            return { ...state, query };
        }
        case ActionType.AdvancedHighlightUpdate: {
            const { highlight } = action.payload;
            return { ...state, highlight };
        }
        default:
            return state;
    }
};

function isAdvancedReducer(state = false, action: Action) {
    switch (action.type) {
        case ActionType.QueryToggle:
            return !state;
        default:
            return state;
    }
}

function resultsReducer(state = [], action: Action) {
    switch (action.type) {
        case ActionType.QueryResults:
            if (!action.error) {
                return action.payload.results;
            } else {
                return state;
            }
        default:
            return state;
    }
}

const initialStructuredQuery = {
    0: { kind: Kind.Containing, lhs: 1, rhs: 2 },
    1: { kind: Kind.Layer, name: 'function' },
    2: { kind: Kind.Term, name: 'ident', value: 'pm' },
};

const makeNothing: () => Nothing = () => ({ kind: Kind.Nothing });

function structuredQueryReducer(state, action: Action) {

    switch (action.type) {
        case ActionType.StructuredQueryKindUpdate: {
            const { kind, id } = action.payload;
            const old = state[id];

            if (old.lhs === undefined || old.rhs === undefined) {
                const lhs = Object.keys(state).length;
                const rhs = lhs + 1;
                return {
                    ...state,
                    [lhs]: makeNothing(),
                    [rhs]: makeNothing(),
                    [id]: { ...old, kind, lhs, rhs }
                };
            } else {
                return { ...state, [id]: { ...old, kind } };
            }
        }
        case ActionType.LayerNameUpdate: {
            const { name, id } = action.payload;
            const old = state[id];

            return { ...state, [id]: { ...old, name } };
        }
        case ActionType.TermNameUpdate: {
            const { name, id } = action.payload;
            const old = state[id];

            return { ...state, [id]: { ...old, name } };
        }
        case ActionType.TermValueUpdate: {
            const { value, id } = action.payload;
            const old = state[id];

            return { ...state, [id]: { ...old, value } };
        }
        default:
            return state;
    }
}

type QueryItem = Term | Containing | Layer | Nothing;

interface Term {
    kind: Kind.Term,
    name: string,
    value: string,
}

// Probably too specific
interface Containing {
    kind: Kind.Containing,
    lhs: QueryItem,
    rhs: QueryItem,
}

interface Layer {
    kind: Kind.Layer,
    name: string,
}

interface Nothing {
    kind: Kind.Nothing,
}

interface QueryItems {
    [index: number]: QueryItem;
}

const initialStructuredHighlight: QueryItems[] = [{
    0: { kind: Kind.Term, name: 'ident', value: 'pm' },
}];

const structuredHighlightReducer = forTarget(forTargetIndex(structuredQueryReducer), 'highlight');
function structuredHighlightsReducer(state = initialStructuredHighlight, action: Action) {
    switch (action.type) {
        case ActionType.HighlightAdd: {
            const { index } = action.payload;
            const newState = state.slice();
            newState.splice(index + 1, 0, { 0: makeNothing() });
            return newState;
        }
        default:
            return structuredHighlightReducer(state, action);
    }
}

export default combineReducers({
    advanced: advancedReducer,
    structuredQuery: initial(forTarget(structuredQueryReducer, 'query'), initialStructuredQuery),
    structuredHighlights: structuredHighlightsReducer,
    isAdvanced: isAdvancedReducer,
    results: resultsReducer,
});
