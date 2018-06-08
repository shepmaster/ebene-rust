import * as React from 'react';
import { connect } from 'react-redux';

import { updateAdvancedQuery, updateAdvancedHighlight } from 'app/actions';
import { State } from 'app/reducer';

interface AdvancedInputProps {
    query: string;
    highlight: string;
    onQueryChange: (string) => any;
    onHighlightChange: (string) => any;
}

const AdvancedInput: React.SFC<AdvancedInputProps> = ({ query, highlight, onQueryChange, onHighlightChange }) => (
    <div className="advanced-input">
        <textarea className="advanced-input__query"
            value={query}
            onChange={e => onQueryChange(e.target.value)}></textarea>
        <textarea className="advanced-input__highlight"
            value={highlight}
            onChange={e => onHighlightChange(e.target.value)}></textarea>
    </div>
);

const mapStateToProps = ({ advanced: { query, highlight } }: State) => ({
    query,
    highlight,
});

const mapDispatchToProps = {
    onQueryChange: updateAdvancedQuery,
    onHighlightChange: updateAdvancedHighlight,
};

export default connect(
    mapStateToProps,
    mapDispatchToProps
)(AdvancedInput);
