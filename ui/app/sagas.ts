import {
    all,
    call,
    fork,
    put,
    select,
    spawn,
    takeEvery,
    takeLatest,
} from 'redux-saga/effects';

import {
    ActionType,
    queryFailed,
    updateAvailableLayers,
    updateAvailableTerms,
    updateQueryResults,
} from './actions';
import { selectQuery } from './selectors';
import search, { fetchLayers, fetchTerms } from './queryApi';

function* performSearch() {
    try {
        const q = yield select(selectQuery);
        const { results } = yield call(search, q);
        yield put(updateQueryResults(results));
    } catch (e) {
        yield put(queryFailed(e.message));
    }
}

function* watchForSearchUpdates() {
    yield takeEvery(ActionType.AdvancedQueryUpdate, performSearch);
    yield takeEvery(ActionType.AdvancedHighlightUpdate, performSearch);
    yield takeEvery(ActionType.StructuredQueryKindUpdate, performSearch);
    yield takeEvery(ActionType.LayerNameUpdate, performSearch);
    yield takeEvery(ActionType.TermNameUpdate, performSearch);
    yield takeEvery(ActionType.TermValueUpdate, performSearch);
}

function* loadInitialData() {
    let [layers, terms] = yield all([
        call(fetchLayers),
        call(fetchTerms),
    ]);
    yield put(updateAvailableLayers(layers));
    yield put(updateAvailableTerms(terms));
}

export default function* main() {
    yield spawn(loadInitialData);
    yield fork(watchForSearchUpdates);
}
