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

export const updateLayerName = (id, name) => ({
  type: constants.LAYER_NAME_UPDATE,
  id, name,
});

export const updateTerminalName = (id, name) => ({
  type: constants.TERMINAL_NAME_UPDATE,
  id, name,
});

export const updateTerminalValue = (id, value) => ({
  type: constants.TERMINAL_VALUE_UPDATE,
  id, value,
});
