import { call, put, select, takeEvery, takeLatest } from 'redux-saga/effects';

import constants from './constants';
import { updateQueryResults } from './actions';
import { selectQuery } from './selectors';
import api from './queryApi';

function* performSearch(action) {
 // try {
  const q = yield select(selectQuery);
  const results = yield call(api, q);
  yield put(updateQueryResults(results.results));
  // } catch (e) {
  //   yield put(queryFailed(e.message));
  // }
}

export default function* mySaga() {
  yield takeEvery(constants.QUERY_TERM_UPDATE, performSearch);
  yield takeEvery(constants.QUERY_WITHIN_UPDATE, performSearch);
  yield takeEvery(constants.ADVANCED_QUERY_UPDATE, performSearch);
  yield takeEvery(constants.ADVANCED_HIGHLIGHT_UPDATE, performSearch);
}
