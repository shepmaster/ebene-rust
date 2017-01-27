import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux';
import { createStore } from 'redux';

import Page from './Page';
import reducer from './reducer';

import './index.scss';

const devtools = window.__REDUX_DEVTOOLS_EXTENSION__ && window.__REDUX_DEVTOOLS_EXTENSION__();
const store = createStore(reducer, devtools);

// function doRender() {
//   var url = `http://127.0.0.1:8080/api/search?`;

//   if (advanced) {
//     url += `q=${advancedQuery}&h=${advancedHighlight}`;
//   } else {
//     if (query !== '') {
//       const structuredQuery = {
//         Containing: [
//           { Layer: { name: within } },
//           { Terminal: { name: "ident", value: query } },
//         ],
//       };

//       url += `q=${JSON.stringify(structuredQuery)}`;

//       const structuredHighlight = [{ Terminal: { name: "ident", value: query } }];

//       url += `&h=${JSON.stringify(structuredHighlight)}`;
//     } else {
//       const structuredQuery = { Layer: { name: within } };
//       url += `q=${JSON.stringify(structuredQuery)}`;
//     }
//   }

//   const searchResults = fetch(url)
//     .then(r => r.json());
// }

ReactDOM.render(
  <Provider store={store}>
    <Page />
  </Provider>,
  document.getElementById('app')
);
