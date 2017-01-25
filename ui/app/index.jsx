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

const Layer = ({ source, extents, index }) => {
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
    <pre className={`layers-layer layers-highlight layers-highlight-${index}`}>
      <code>
        { pieces }
      </code>
    </pre>
  );
};

const Result = ({ source, highlights }) => {
  const layers = highlights.map((highlight, i) => (
    <Layer key={i} source={source} extents={highlight} index={i} />
  ));

  return (
    <div className="layers">
      <Code source={source} />
      { layers }
    </div>
  );
};

const ResultList = ({ results }) => {
  const renderedResults = results.map(({ text, highlights }, i) => (
    <li key={i}><Result source={text} highlights={highlights} /></li>
  ));

  return <ol className="results">{ renderedResults }</ol>;
};

const SimpleInput = ({ onQueryChange, onContainerChange }) => (
  <div>
    <input onChange={e => onQueryChange(e.target.value)}></input>
    <select onChange={e => onContainerChange(e.target.value)}>
      <option value="function">Function</option>
      <option value="enum">Enum</option>
      <option value="struct">Struct</option>
    </select>
  </div>
);

const AdvancedInput = ({ onAdvancedQueryChange, onAdvancedHighlightChange }) => (
  <div className="advanced-input">
    <textarea className="advanced-input__query" onChange={e => onAdvancedQueryChange(e.target.value)}></textarea>
    <textarea className="advanced-input__highlight" onChange={e => onAdvancedHighlightChange(e.target.value)}></textarea>
  </div>
);

const Input = ({ advanced, onQueryChange, onContainerChange, onAdvancedQueryChange, onAdvancedHighlightChange }) => {
  if (advanced) {
    return (
      <AdvancedInput onAdvancedQueryChange={onAdvancedQueryChange}
                     onAdvancedHighlightChange={onAdvancedHighlightChange} />
    );
  } else {
    return (
      <SimpleInput onQueryChange={onQueryChange}
                   onContainerChange={onContainerChange} />
    );
  }
};

const Page = ({ results, advanced, toggleAdvanced, onQueryChange, onContainerChange, onAdvancedQueryChange, onAdvancedHighlightChange }) => (
  <div>
    <button onClick={toggleAdvanced}>Mode</button>
    <Input advanced={advanced}
           onQueryChange={onQueryChange}
           onContainerChange={onContainerChange}
           onAdvancedQueryChange={onAdvancedQueryChange}
           onAdvancedHighlightChange={onAdvancedHighlightChange} />
    <ResultList results={results} />
  </div>
);

var query = '';
var within = 'function';
var advanced = false;
var advancedQuery = '{"Layer": {"name": "function"}}';
var advancedHighlight = '{"Terminal": {"name": "ident", "value": "pm"}}';

function updateQuery(v) {
  query = v;
  doRender();
}

function updateWithin(v) {
  within = v;
  doRender();
}

function toggleAdvanced() {
  advanced = !advanced;
  doRender();
}

function updateAdvancedQuery(v) {
  advancedQuery = v;
  doRender();
}

function updateAdvancedHighlight(v) {
  advancedHighlight = v;
  doRender();
}

function doRender() {
  var url = `http://127.0.0.1:8080/api/search?`;

  if (advanced) {
    url += `q=${advancedQuery}&h=${advancedHighlight}`;
  } else {
    if (query !== '') {
      const structuredQuery = {
        Containing: [
          { Layer: { name: within } },
          { Terminal: { name: "ident", value: query } },
        ],
      };

      url += `q=${JSON.stringify(structuredQuery)}`;

      const structuredHighlight = [{ Terminal: { name: "ident", value: query } }];

      url += `&h=${JSON.stringify(structuredHighlight)}`;
    } else {
      const structuredQuery = { Layer: { name: within } };
      url += `q=${JSON.stringify(structuredQuery)}`;
    }
  }

  const searchResults = fetch(url)
    .then(r => r.json());

  searchResults.then(({ results }) => {
    ReactDOM.render(
      <Page advanced={advanced}
            results={results}
            toggleAdvanced={toggleAdvanced}
            onQueryChange={updateQuery}
            onContainerChange={updateWithin}
            onAdvancedQueryChange={updateAdvancedQuery}
            onAdvancedHighlightChange={updateAdvancedHighlight} />,
      document.getElementById('app')
    );
  });
}

doRender();
