import React from 'react';
import { connect } from 'react-redux';

import { selectTreeQuery } from './selectors';
import { updateStructuredQueryKind, updateLayerName, updateTerminalName, updateTerminalValue } from './actions';

const SelectKind = ({ id, kind, onKindChange }) => {
  const options = componentKinds.map((name, i) => (
    <option key={i} value={name}>{name}</option>
  ));

  return (
    <select value={kind} onChange={e => onKindChange(id, e.target.value)}>
      { options }
    </select>
  );
};

const Nothing = ({ id, kind, handlers: { onKindChange } }) => (
  <div>
    <SelectKind id={id} kind={kind} onKindChange={onKindChange} />
  </div>
);

const Layer = ({ id, kind, name, handlers: { onKindChange, onLayerChange } }) => (
  <div>
    <SelectKind id={id} kind={kind} onKindChange={onKindChange} />
    <input value={name} onChange={e => onLayerChange(id, e.target.value)}></input>
  </div>
);

const Terminal = ({ id, kind, name, value, handlers: { onKindChange, onTerminalNameChange, onTerminalValueChange } }) => (
  <div>
    <SelectKind id={id} kind={kind} onKindChange={onKindChange} />
    <input value={name} onChange={e => onTerminalNameChange(id, e.target.value)}></input>
    :
    <input value={value} onChange={e => onTerminalValueChange(id, e.target.value)}></input>
  </div>
);

const BinaryComponent = ({ id, kind, lhs, rhs, handlers }) => {
  const { onKindChange } = handlers;

  return (
    <div>
      <SelectKind id={id} kind={kind} onKindChange={onKindChange} />
      <div className="structured-query__child">
        <QueryBuilder handlers={handlers} {...lhs} />
        <QueryBuilder handlers={handlers} {...rhs} />
      </div>
    </div>
  );
};

const mapKindToComponent = {
  'Nothing': Nothing,
  'Layer': Layer,
  'Terminal': Terminal,
  'Containing': BinaryComponent,
  'ContainedIn': BinaryComponent,
  'NotContaining': BinaryComponent,
  'NotContainedIn': BinaryComponent,
  'OneOf': BinaryComponent,
  'BothOf': BinaryComponent,
  'FollowedBy': BinaryComponent,
};

const componentKinds = Object.keys(mapKindToComponent);

const QueryBuilder = (props) => {
  const Component = mapKindToComponent[props.kind];
  return (
    <div className="structured-query">
      <Component {...props} />
    </div>
  );
};

const mapStateToProps = (state) => selectTreeQuery(state);

const mapDispatchToProps = (dispatch) => ({
  handlers: {
    onKindChange: (id, k) => dispatch(updateStructuredQueryKind(id, k)),
    onLayerChange: (id, n) => dispatch(updateLayerName(id, n)),
    onTerminalNameChange: (id, n) => dispatch(updateTerminalName(id, n)),
    onTerminalValueChange: (id, v) => dispatch(updateTerminalValue(id, v)),
  }
});

export default connect(
  mapStateToProps,
  mapDispatchToProps
)(QueryBuilder);
