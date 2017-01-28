import React from 'react';
import { connect } from 'react-redux';

import QueryEditor from './QueryEditor';
import { selectTreeQuery } from './selectors';
import { updateQueryKind, updateQueryLayerName, updateQueryTerminalName, updateQueryTerminalValue } from './actions';

const mapStateToProps = (state) => selectTreeQuery(state.structuredQuery);

const mapDispatchToProps = (dispatch) => ({
  handlers: {
    onKindChange: (id, k) => dispatch(updateQueryKind(id, k)),
    onLayerChange: (id, n) => dispatch(updateQueryLayerName(id, n)),
    onTerminalNameChange: (id, n) => dispatch(updateQueryTerminalName(id, n)),
    onTerminalValueChange: (id, v) => dispatch(updateQueryTerminalValue(id, v)),
  }
});

export default connect(
  mapStateToProps,
  mapDispatchToProps
)(QueryEditor);
