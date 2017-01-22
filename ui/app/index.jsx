import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';

const Code = ({ source }) => (
  <pre><code>{ source }</code></pre>
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
    <pre>
      <code>
        { pieces }
      </code>
    </pre>
  );
};

const Page = ({ source, extents }) => (
  <div className="layers">
    <Code source={source} />
    <Layer source={source} extents={extents} />
  </div>
);


function doRender() {
  const query = (window.location.hash || '#peresil').slice(1);

  const source = fetch("http://127.0.0.1:8080/api")
    .then(r => r.json());

  const extents = fetch(`http://127.0.0.1:8080/api/idents/${query}`)
    .then(r => r.json());

  const data = Promise.all([source, extents])
    .then(([source, extents]) => ({ source: source.source, extents: extents.extents }));

  data.then(({ source, extents }) => {
    ReactDOM.render(
      <Page source={source} extents={extents} />,
      document.getElementById('app')
    );
  });
}

window.onhashchange = doRender;
doRender();
