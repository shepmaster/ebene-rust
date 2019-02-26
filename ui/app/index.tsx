import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { Provider } from 'react-redux';
import { createStore, applyMiddleware, compose } from 'redux';
import createSagaMiddleware from 'redux-saga';

import Page from './Page';
import reducer from './reducer';
import saga from './sagas';

import 'normalize.css';
import './index.scss';

const sagaMiddleware = createSagaMiddleware();

const composeEnhancers = (window as any).__REDUX_DEVTOOLS_EXTENSION_COMPOSE__ || compose;
const store = createStore(reducer, {}, composeEnhancers(
    applyMiddleware(sagaMiddleware)
));

sagaMiddleware.run(saga);

ReactDOM.render(
    <Provider store={store}>
        <Page />
    </Provider>,
    document.getElementById('app')
);
