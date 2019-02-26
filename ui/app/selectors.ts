import { Kind, BinaryKind, FlatQueryItems, TreeQueryItem, ApiQueryItem, ApiBinary } from './types';
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
    function treeify(id: number): ApiQueryItem {
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
                return { [thisQuery.kind]: [treeify(lhs), treeify(rhs)] } as ApiBinary<BinaryKind>;
            }
            case Kind.Layer: {
                const { kind, name } = thisQuery;
                return { [thisQuery.kind]: { name } };
            }
            case Kind.Term: {
                const { kind, name, value } = thisQuery;
                return { [thisQuery.kind]: { name, value } };
            }
            case Kind.Nothing:
                return thisQuery.kind;
        };
    }

    return treeify(0);
}

export function selectTreeQuery(queryList: FlatQueryItems) {
    function treeify(id: number): TreeQueryItem {
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
