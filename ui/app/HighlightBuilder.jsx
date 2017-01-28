import React from 'react';
import { connect } from 'react-redux';
import { bindActionCreators } from 'redux';

import QueryEditor from './QueryEditor';
import { selectTreeQuery } from './selectors';
import { updateKind, updateLayerName, updateTerminalName, updateTerminalValue, retarget } from './actions';

const mapStateToProps = (state) => selectTreeQuery(state.structuredHighlight);

const targetMe = (action) => retarget(action, 'highlight');

const mapDispatchToProps = (dispatch) => ({
  handlers: bindActionCreators({
    onKindChange: targetMe(updateKind),
    onLayerChange: targetMe(updateLayerName),
    onTerminalNameChange: targetMe(updateTerminalName),
    onTerminalValueChange: targetMe(updateTerminalValue),
  }, dispatch)
});

export default connect(
  mapStateToProps,
  mapDispatchToProps
)(QueryEditor);
