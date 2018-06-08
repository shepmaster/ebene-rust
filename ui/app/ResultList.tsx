import * as React from 'react';
import { QueryResult, Extents } from './types';

interface CodeProps {
    source: string;
}

const Code: React.SFC<CodeProps> = ({ source }) => (
    <pre className="layers-layer">
        <code>{source}</code>
    </pre>
);

function* flattenExtents(extents: Extents) {
    for (const [start, end] of extents) {
        yield start;
        yield end;
    }
}

function* sourceCutpoints(source: string, extents: Extents) {
    yield 0;
    yield* flattenExtents(extents);
    yield source.length;
}

function* highlightedSourceWindows(source: string, extents: Extents): IterableIterator<[boolean, string]> {
    const p = sourceCutpoints(source, extents);
    let start: number | undefined = undefined;
    let end: number | undefined = undefined;
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

interface LayerProps {
    source: string;
    extents: Extents;
    index: number;
}

const Layer: React.SFC<LayerProps> = ({ source, extents, index }) => {
    const pieces: (string | JSX.Element)[] = [];
    let count = 0;

    for (const [highlight, text] of highlightedSourceWindows(source, extents)) {
        if (highlight) {
            pieces.push(<span key={count} className="highlight">{text}</span>);
        } else {
            pieces.push(text);
        }
        count++;
    }

    return (
        <pre className={`layers-layer layers-highlight layers-highlight-${index}`}>
            <code>
                {pieces}
            </code>
        </pre>
    );
};

interface ResultProps {
    source: QueryResult['text'];
    highlights: QueryResult['highlights'];
}

const Result: React.SFC<ResultProps> = ({ source, highlights }) => {
    const layers = highlights.map((highlight, i) => (
        <Layer key={i} source={source} extents={highlight} index={i} />
    ));

    return (
        <div className="layers">
            <Code source={source} />
            {layers}
        </div>
    );
};

interface ResultListProps {
    results: QueryResult[];
}

const ResultList: React.SFC<ResultListProps> = ({ results }) => {
    const renderedResults = results.map(({ text, highlights }, i) => (
        <li key={i}><Result source={text} highlights={highlights} /></li>
    ));

    return (
        <div>
            Showing {results.length} results:
            <ol className="results">{renderedResults}</ol>
        </div>
    );
};

export default ResultList;
