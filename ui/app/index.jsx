import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';

const Code = ({ source }) => (
  <pre className="layers-layer">
    <code>{ source }</code>
  </pre>
);

function *flattenExtents(extents) {
  for (const [start, end] of extents) {
    yield start;
    yield end;
  }
}

function *sourceCutpoints(source, extents) {
  yield 0;
  yield *flattenExtents(extents);
  yield source.length;
}

function *highlightedSourceWindows(source, extents) {
  const p = sourceCutpoints(source, extents);
  let start = undefined;
  let end = undefined;
  let highlight = false;

  for (const i of p) {
    start = end;
    end = i;
    if (start === undefined || end == undefined) { continue; }

    const s = source.slice(start, end);
    yield [highlight, s];
    highlight = !highlight;
  }
}

const Layer = ({ source, extents }) => {
  const pieces = [];
  let count = 0;

  for (const [highlight, text] of highlightedSourceWindows(source, extents)) {
    if (highlight) {
      pieces.push(<span key={count} className="highlight">{ text }</span>);
    } else {
      pieces.push(text);
    }
    count++;
  }

  return (
    <pre className="layers-layer layers-highlight">
      <code>
        { pieces }
      </code>
    </pre>
  );
};

const Result = ({ source, extents }) => (
  <div className="layers">
    <Code source={source} />
    <Layer source={source} extents={extents} />
  </div>
);

const ResultList = ({ results }) => {
  const renderedResults = results.map(({ text, highlight }, i) => (
    <li key={i}><Result source={text} extents={highlight} /></li>
  ));

  return <ol className="results">{ renderedResults }</ol>;
};

const Page = ({ results, onQueryChange, onContainerChange }) => (
  <div>
    <input onChange={e => onQueryChange(e.target.value)}></input>
    <select onChange={e => onContainerChange(e.target.value)}>
      <option value="function">Function</option>
      <option value="enum">Enum</option>
      <option value="struct">Struct</option>
    </select>
    <ResultList results={results} />
  </div>
);

var query = '';
var within = 'function';

function updateQuery(v) {
  query = v;
  doRender();
}

function updateWithin(v) {
  within = v;
  doRender();
}

function doRender() {
  var url = `http://127.0.0.1:8080/api/search?`;

  if (query !== '') {
    const structuredQuery = {
      Containing: [
        { Layer: { name: within } },
        { Terminal: { name: "ident", value: query } },
      ],
    };

    url += `q=${JSON.stringify(structuredQuery)}`;

    const structuredHighlight = { Terminal: { name: "ident", value: query } };

    url += `&h=${JSON.stringify(structuredHighlight)}`;
  } else {
    const structuredQuery = { Layer: { name: within } };
    url += `q=${JSON.stringify(structuredQuery)}`;
  }

  const searchResults = fetch(url)
    .then(r => r.json());

  searchResults.then(({ results }) => {
    ReactDOM.render(
      <Page results={results} onQueryChange={updateQuery} onContainerChange={updateWithin} />,
      document.getElementById('app')
    );
  });
}

doRender();
