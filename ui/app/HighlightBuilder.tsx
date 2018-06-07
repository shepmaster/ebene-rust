import * as React from 'react';
import { connect } from 'react-redux';
import { bindActionCreators } from 'redux';

import QueryEditor from './QueryEditor';
import { selectTreeQuery } from './selectors';
import {
    updateKind, updateLayerName, updateTermName, updateTermValue,
    highlightAdd,
    retarget, retargetIndex
} from './actions';

// Extracting `index` to prevent passing it down further
const HighlightOneUnconnected = ({ index, onAddHighlight, ...props }) => (
    <div>
        <button onClick={onAddHighlight} />
        <QueryEditor {...props} />
    </div>
);

const targetMe = (action, index) => retarget(retargetIndex(action, index), 'highlight');

const mapDispatchToProps = (dispatch, { index }) => ({
    handlers: bindActionCreators({
        onKindChange: targetMe(updateKind, index),
        onLayerChange: targetMe(updateLayerName, index),
        onTermNameChange: targetMe(updateTermName, index),
        onTermValueChange: targetMe(updateTermValue, index),
    }, dispatch),
    onAddHighlight: () => dispatch(highlightAdd(index)),
});

const HighlightOne = connect(
    null,
    mapDispatchToProps
)(HighlightOneUnconnected);



const Highlights = ({ highlights }) => {
    const rendered = highlights.map((h, i) => <HighlightOne key={i} index={i} {...h} />);
    return <div>{rendered}</div>;
};

const mapStateToProps = (state) => ({
    highlights: state.structuredHighlights.map(selectTreeQuery),
});

export default connect(
    mapStateToProps,
    null
)(Highlights);
