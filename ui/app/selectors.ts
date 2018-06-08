import { Kind, BinaryKind, FlatQueryItems } from './types';
import { State } from './reducer';

export function selectQuery(state: State) {
    if (state.isAdvanced) {
        const { query, highlight } = state.advanced;
        return {
            q: query,
            h: highlight,
        };
    }

    const structuredQuery = selectTreeQueryForApi(state.structuredQuery);
    const structuredHighlight = state.structuredHighlights.map(selectTreeQueryForApi);

    return {
        q: JSON.stringify(structuredQuery),
        h: JSON.stringify(structuredHighlight),
    };
}

function selectTreeQueryForApi(queryList: FlatQueryItems) {
    function treeify(id: number) {
        let thisQuery = queryList[id];

        switch (thisQuery.kind) {
            case Kind.Containing:
            case Kind.ContainedIn:
            case Kind.NotContaining:
            case Kind.NotContainedIn:
            case Kind.OneOf:
            case Kind.BothOf:
            case Kind.FollowedBy: {
                let { lhs, rhs } = thisQuery;
                return { [thisQuery.kind]: [treeify(lhs), treeify(rhs)] };
            }
            case Kind.Nothing:
                return "Nothing";
            default: {
                const { kind, ...rest } = thisQuery;
                return { [thisQuery.kind]: rest };
            }
        };
    }

    return treeify(0);
}

export function selectTreeQuery(queryList: FlatQueryItems) {
    function treeify(id: number) {
        const thisQuery = queryList[id];

        switch (thisQuery.kind) {
            case Kind.Containing:
            case Kind.ContainedIn:
            case Kind.NotContaining:
            case Kind.NotContainedIn:
            case Kind.OneOf:
            case Kind.BothOf:
            case Kind.FollowedBy:
                return { ...thisQuery, id, lhs: treeify(thisQuery.lhs), rhs: treeify(thisQuery.rhs) };
            default:
                return { ...thisQuery, id };
        }
    }

    return treeify(0);
}

export const selectAvailableTerms = (state: State) => state.available.terms;
export const selectAvailableLayers = (state: State) => state.available.layers;

export const selectTermValid = (state: State, term: string) =>
    selectAvailableTerms(state).includes(term);
export const selectLayerValid = (state: State, layer: string) =>
    selectAvailableLayers(state).includes(layer);
