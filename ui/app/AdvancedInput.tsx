import * as React from 'react';
import { connect } from 'react-redux';

import { updateAdvancedQuery, updateAdvancedHighlight } from './actions';

const AdvancedInput = ({ query, highlight, onQueryChange, onHighlightChange }) => (
    <div className="advanced-input">
        <textarea className="advanced-input__query"
            value={query}
            onChange={e => onQueryChange(e.target.value)}></textarea>
        <textarea className="advanced-input__highlight"
            value={highlight}
            onChange={e => onHighlightChange(e.target.value)}></textarea>
    </div>
);

const mapStateToProps = ({ advanced: { query, highlight } }) => ({
    query,
    highlight,
});

const mapDispatchToProps = (dispatch) => ({
    onQueryChange: q => dispatch(updateAdvancedQuery(q)),
    onHighlightChange: h => dispatch(updateAdvancedHighlight(h)),
});

export default connect(
    mapStateToProps,
    mapDispatchToProps
)(AdvancedInput);
