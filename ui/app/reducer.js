import { combineReducers } from 'redux';
import constants from './constants';

const initialAdvState = {
  query: '{"Layer": {"name": "function"}}',
  highlight: '[{"Terminal": {"name": "ident", "value": "pm"}}]',
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
  switch (action.type) {
  case constants.QUERY_RESULTS_SUCCESS:
    return action.results;
  default:
    return state;
  }
}

const initialStructuredQuery = {
  0: { kind: 'Containing', lhs: 1, rhs: 2 },
  1: { kind: 'Layer', name: 'function' },
  2: { kind: 'Terminal', name: 'ident', value: 'pm' },
};

function structuredQueryReducer(state = initialStructuredQuery, action) {
  const { id } = action;
  const old = state[id];

  switch (action.type) {
  case constants.STRUCTURED_QUERY_KIND_UPDATE: {
    const { kind } = action;
    if (old.lhs === undefined || old.rhs === undefined) {
      const lhs = Object.keys(state).length;
      const rhs = lhs + 1;
      return {
        ...state,
        [lhs]: { kind: 'Nothing' },
        [rhs]: { kind: 'Nothing' },
        [id]: { ...old, kind, lhs, rhs }
      };
    } else {
      return { ...state, [id]: { ...old, kind } };
    }
  }
  case constants.LAYER_NAME_UPDATE: {
    const { name } = action;
    return { ...state, [id]: { ...old, name } };
  }
  case constants.TERMINAL_NAME_UPDATE: {
    const { name } = action;
    return { ...state, [id]: { ...old, name } };
  }
  case constants.TERMINAL_VALUE_UPDATE: {
    const { value } = action;
    return { ...state, [id]: { ...old, value } };
  }
  default:
    return state;
  }
}

export default combineReducers({
  advanced: advancedReducer,
  structuredQuery: structuredQueryReducer,
  isAdvanced: isAdvancedReducer,
  results: resultsReducer,
});
