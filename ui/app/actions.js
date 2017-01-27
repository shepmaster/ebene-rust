import constants from './constants';

export const updateQuery = (query) => ({
  type: constants.QUERY_TERM_UPDATE,
  query,
});

export const updateWithin = (within) => ({
  type: constants.QUERY_WITHIN_UPDATE,
  within
});

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
