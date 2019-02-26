import constants from './constants';

export const toggleAdvanced = () => ({
    type: constants.QUERY_TOGGLE,
});

export const updateAdvancedQuery = (query) => ({
    type: constants.ADVANCED_QUERY_UPDATE,
    query,
});

export const updateAdvancedHighlight = (highlight) => ({
    type: constants.ADVANCED_HIGHLIGHT_UPDATE,
    highlight,
});

export const updateQueryResults = (results) => ({
    type: constants.QUERY_RESULTS_SUCCESS,
    results,
});

export const queryFailed = (message) => ({
    type: constants.QUERY_RESULTS_FAILURE,
    message,
});

export const updateKind = (id, kind) => ({
    type: constants.STRUCTURED_QUERY_KIND_UPDATE,
    id, kind
});

export const updateLayerName = (id, name) => ({
    type: constants.LAYER_NAME_UPDATE,
    id, name
});

export const updateTermName = (id, name) => ({
    type: constants.TERM_NAME_UPDATE,
    id, name
});

export const updateTermValue = (id, value) => ({
    type: constants.TERM_VALUE_UPDATE,
    id, value
});

export const highlightAdd = (index) => ({
    type: constants.HIGHLIGHT_ADD,
    index
});

export const retarget = (action, target) => (...args) => {
    let createdAction = action(...args);
    createdAction.target = target;
    return createdAction;
};

export const retargetIndex = (action, index) => (...args) => {
    let createdAction = action(...args);
    createdAction.targetIndex = index;
    return createdAction;
};
