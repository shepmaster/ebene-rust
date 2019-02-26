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

function selectTreeQueryForApi(queryList) {
    function treeify(id) {
        let { kind, ...rest } = queryList[id];
        switch (kind) {
            case 'Containing':
            case 'ContainedIn':
            case 'NotContaining':
            case 'NotContainedIn':
            case 'OneOf':
            case 'BothOf':
            case 'FollowedBy':
                return { [kind]: [treeify(rest.lhs), treeify(rest.rhs)] };
            default:
                return { [kind]: rest };
        };
    }

    return treeify(0);
}

export function selectTreeQuery(queryList) {
    function treeify(id) {
        const thisQuery = queryList[id];

        switch (thisQuery.kind) {
            case 'Containing':
            case 'ContainedIn':
            case 'NotContaining':
            case 'NotContainedIn':
            case 'OneOf':
            case 'BothOf':
            case 'FollowedBy':
                return { ...thisQuery, id, lhs: treeify(thisQuery.lhs), rhs: treeify(thisQuery.rhs) };
            default:
                return { ...thisQuery, id };
        }
    }

    return treeify(0);
}

export const selectAvailableTerms = (state) => state.available.terms;
export const selectAvailableLayers = (state) => state.available.layers;
