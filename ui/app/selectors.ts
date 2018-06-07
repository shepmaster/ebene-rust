import { Kind, BinaryKind } from './types';

type QueryTerm = BinaryQueryTerm | OtherQueryTerm;

interface BinaryQueryTerm {
    kind: BinaryKind,
    lhs: number;
    rhs: number;
}

interface OtherQueryTerm {
    kind: Kind.Layer | Kind.Nothing | Kind.Term,
}

export function selectQuery(state) {
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

function selectTreeQueryForApi(queryList: QueryTerm[]) {
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

export function selectTreeQuery(queryList: QueryTerm[]) {
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

export const selectAvailableTerms = (state) => state.available.terms;
export const selectAvailableLayers = (state) => state.available.layers;

export const selectTermValid = (state, term) => selectAvailableTerms(state).includes(term);
export const selectLayerValid = (state, layer) => selectAvailableLayers(state).includes(layer);
