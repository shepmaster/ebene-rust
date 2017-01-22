import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';

const source = fetch("http://127.0.0.1:8080/api")
      .then(r => r.json());

const query = (window.location.hash || '#peresil').slice(1);

const extents = fetch(`http://127.0.0.1:8080/api/idents/${query}`)
      .then(r => r.json());

const data = Promise.all([source, extents])
      .then(([source, extents]) => ({ source: source.source, extents: extents.extents }));

const Code = ({ source }) => (
  <pre><code>{ source }</code></pre>
);

const Layer = ({ source, extents }) => {
  const [start, end] = extents[0];
  let head = source.slice(0, start);
  let middle = source.slice(start, end);
  let tail = source.slice(end);

  return (
    <pre>
      <code>
        { head }
        <span className="highlight">{ middle }</span>
        { tail }
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

data.then(({ source, extents }) => {
  ReactDOM.render(
    <Page source={source} extents={extents} />,
    document.getElementById('app')
  );
});
