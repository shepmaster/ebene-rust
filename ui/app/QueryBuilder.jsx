import React from 'react';
import { connect } from 'react-redux';

import { selectTreeQuery } from './selectors';
import { updateLayerName, updateTerminalName, updateTerminalValue } from './actions';

const Layer = ({ id, name, onLayerChange }) => (
  <div>
    <b>Layer</b>
    <input value={name} onChange={e => onLayerChange(id, e.target.value)}></input>
  </div>
);

const Terminal = ({ id, name, value, onTerminalNameChange, onTerminalValueChange }) => (
  <div>
    <b>Terminal</b>
    <input value={name} onChange={e => onTerminalNameChange(id, e.target.value)}></input>
    :
    <input value={value} onChange={e => onTerminalValueChange(id, e.target.value)}></input>
  </div>
);

const Containing = ({ lhs, rhs, ...other }) => (
  <div>
    <b>Containing</b>
    <QueryBuilder {...other} {...lhs} />
    <QueryBuilder {...other} {...rhs} />
  </div>
);

const mapKindToComponent = {
  'Layer': Layer,
  'Terminal': Terminal,
  'Containing': Containing,
};

const QueryBuilder = (props) => {
  const Component = mapKindToComponent[props.kind];
  return <Component {...props} />;
};

const mapStateToProps = (state) => selectTreeQuery(state);

const mapDispatchToProps = (dispatch) => ({
  onLayerChange: (id, n) => dispatch(updateLayerName(id, n)),
  onTerminalNameChange: (id, n) => dispatch(updateTerminalName(id, n)),
  onTerminalValueChange: (id, v) => dispatch(updateTerminalValue(id, v)),
});

export default connect(
  mapStateToProps,
  mapDispatchToProps
)(QueryBuilder);
