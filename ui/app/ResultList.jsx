import React from 'react';

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

export default ResultList;
