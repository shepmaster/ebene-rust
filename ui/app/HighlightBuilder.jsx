import React from 'react';
import { connect } from 'react-redux';
import { bindActionCreators } from 'redux';

import QueryEditor from './QueryEditor';
import { selectTreeQuery } from './selectors';
import { updateKind, updateLayerName, updateTerminalName, updateTerminalValue, retarget, retargetIndex } from './actions';


const targetMe = (action, index) => retarget(retargetIndex(action, index), 'highlight');

const mapDispatchToProps = (dispatch, { index }) => ({
  handlers: bindActionCreators({
    onKindChange: targetMe(updateKind, index),
    onLayerChange: targetMe(updateLayerName, index),
    onTerminalNameChange: targetMe(updateTerminalName, index),
    onTerminalValueChange: targetMe(updateTerminalValue, index),
  }, dispatch)
});

const HighlightOne = connect(
  null,
  mapDispatchToProps
)(QueryEditor);



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
