import { combineReducers } from 'redux';
import constants from './constants';

const initialSimpleState = {
  query: '',
  within: 'function',
};

function simpleReducer(state = initialSimpleState, action) {
  switch (action.type) {
  case constants.QUERY_TERM_UPDATE: {
    const { query } = action;
    return { ...state, query };
  }
  case constants.QUERY_WITHIN_UPDATE: {
    const { within } = action;
    return { ...state, within };
  }
  default:
    return state;
  }
}

const initialAdvState = {
  query: '{"Layer": {"name": "function"}}',
  highlight: '{"Terminal": {"name": "ident", "value": "pm"}}',
};

function advancedReducer(state = initialAdvState, action) {
  switch (action.type) {
  case constants.ADVANCED_QUERY_UPDATE: {
    const { query } = action;
    return { ...state, query };
  }
  case constants.ADVANCED_HIGHLIGHT_UPDATE: {
    const { highlight } = action;
    return { ...state, highlight };
  }
  default:
    return state;
  }
};

function isAdvancedReducer(state = false, action) {
  switch (action.type) {
  case constants.QUERY_TOGGLE:
    return !state;
  default:
    return state;
  }
}

function resultsReducer(state = [], action) {
  return state;
}

export default combineReducers({
  simple: simpleReducer,
  advanced: advancedReducer,
  isAdvanced: isAdvancedReducer,
  results: resultsReducer,
});
