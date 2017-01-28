import React from 'react';
import { connect } from 'react-redux';

import { QueryBuilder } from './QueryBuilder';
import { selectTreeQuery } from './selectors';
import { updateHighlightKind, updateHighlightLayerName, updateHighlightTerminalName, updateHighlightTerminalValue } from './actions';

const mapStateToProps = (state) => selectTreeQuery(state.structuredHighlight);

const mapDispatchToProps = (dispatch) => ({
  handlers: {
    onKindChange: (id, k) => dispatch(updateHighlightKind(id, k)),
    onLayerChange: (id, n) => dispatch(updateHighlightLayerName(id, n)),
    onTerminalNameChange: (id, n) => dispatch(updateHighlightTerminalName(id, n)),
    onTerminalValueChange: (id, v) => dispatch(updateHighlightTerminalValue(id, v)),
  }
});

export default connect(
  mapStateToProps,
  mapDispatchToProps
)(QueryBuilder);
