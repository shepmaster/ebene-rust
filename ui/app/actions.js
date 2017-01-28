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

const structuredQueryActions = (target) => ({
  updateKind: (id, kind) => ({
    type: constants.STRUCTURED_QUERY_KIND_UPDATE,
    id, kind, target
  }),

  updateLayerName: (id, name) => ({
    type: constants.LAYER_NAME_UPDATE,
    id, name, target
  }),

  updateTerminalName: (id, name) => ({
    type: constants.TERMINAL_NAME_UPDATE,
    id, name, target
  }),

  updateTerminalValue: (id, value) => ({
    type: constants.TERMINAL_VALUE_UPDATE,
    id, value, target
  })
});

const forQuery = structuredQueryActions('query');

export const updateQueryKind = forQuery.updateKind;
export const updateQueryLayerName = forQuery.updateLayerName;
export const updateQueryTerminalName = forQuery.updateTerminalName;
export const updateQueryTerminalValue = forQuery.updateTerminalValue;

const forHighlight = structuredQueryActions('highlight');

export const updateHighlightKind = forHighlight.updateKind;
export const updateHighlightLayerName = forHighlight.updateLayerName;
export const updateHighlightTerminalName = forHighlight.updateTerminalName;
export const updateHighlightTerminalValue = forHighlight.updateTerminalValue;
