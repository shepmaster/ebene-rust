import { combineReducers } from 'redux';

import { Kind } from 'app/types';
import { ActionType, Action } from 'app/actions';

import advanced, { State as AdvancedState } from './reducer/advanced';
import available, { State as AvailableState } from './reducer/available';
import isAdvanced, { State as IsAdvancedState } from './reducer/isAdvanced';
import results, { State as ResultsState } from './reducer/results';
import structuredHighlights, { State as StructuredHighlightsState } from './reducer/structuredHighlights';
import structuredQuery, { State as StructuredQueryState } from './reducer/structuredQuery';

export interface State {
    advanced: AdvancedState,
    available: AvailableState,
    isAdvanced: IsAdvancedState,
    results: ResultsState,
    structuredHighlights: StructuredHighlightsState,
    structuredQuery: StructuredQueryState,
}

export default combineReducers({
    advanced,
    available,
    isAdvanced,
    results,
    structuredHighlights,
    structuredQuery,
});
