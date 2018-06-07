import { call, put, select, takeEvery, takeLatest } from 'redux-saga/effects';

import { ActionType, updateQueryResults, queryFailed } from './actions';
import { selectQuery } from './selectors';
import api from './queryApi';

function* performSearch(action) {
    try {
        const q = yield select(selectQuery);
        const results = yield call(api, q);
        yield put(updateQueryResults(results.results));
    } catch (e) {
        yield put(queryFailed(e.message));
    }
}

export default function* mySaga() {
    yield takeEvery(ActionType.AdvancedQueryUpdate, performSearch);
    yield takeEvery(ActionType.AdvancedHighlightUpdate, performSearch);
    yield takeEvery(ActionType.StructuredQueryKindUpdate, performSearch);
    yield takeEvery(ActionType.LayerNameUpdate, performSearch);
    yield takeEvery(ActionType.TermNameUpdate, performSearch);
    yield takeEvery(ActionType.TermValueUpdate, performSearch);
}
