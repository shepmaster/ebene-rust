import { combineReducers } from 'redux';

import { Kind } from './types';
import { ActionType, Action } from './actions';

import advancedReducer from './reducer/advanced';
import isAdvancedReducer from './reducer/isAdvanced';
import resultsReducer from './reducer/results';
import availableReducer from './reducer/available';
import structuredQueryReducer from './reducer/structuredQuery';
import structuredHighlightsReducer from './reducer/structuredHighlights';

export default combineReducers({
    advanced: advancedReducer,
    available: availableReducer,
    structuredQuery: structuredQueryReducer,
    structuredHighlights: structuredHighlightsReducer,
    isAdvanced: isAdvancedReducer,
    results: resultsReducer,
});
