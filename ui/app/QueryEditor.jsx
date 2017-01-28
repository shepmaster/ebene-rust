import React from 'react';

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

const Term = ({ id, kind, name, value, handlers: { onKindChange, onTermNameChange, onTermValueChange } }) => (
  <div>
    <SelectKind id={id} kind={kind} onKindChange={onKindChange} />
    <input value={name} onChange={e => onTermNameChange(id, e.target.value)}></input>
    :
    <input value={value} onChange={e => onTermValueChange(id, e.target.value)}></input>
  </div>
);

const BinaryComponent = ({ id, kind, lhs, rhs, handlers }) => {
  const { onKindChange } = handlers;

  return (
    <div>
      <SelectKind id={id} kind={kind} onKindChange={onKindChange} />
      <div className="structured-query__child">
        <QueryEditor handlers={handlers} {...lhs} />
        <QueryEditor handlers={handlers} {...rhs} />
      </div>
    </div>
  );
};

const mapKindToComponent = {
  'Nothing': Nothing,
  'Layer': Layer,
  'Term': Term,
  'Containing': BinaryComponent,
  'ContainedIn': BinaryComponent,
  'NotContaining': BinaryComponent,
  'NotContainedIn': BinaryComponent,
  'OneOf': BinaryComponent,
  'BothOf': BinaryComponent,
  'FollowedBy': BinaryComponent,
};

const componentKinds = Object.keys(mapKindToComponent);

const QueryEditor = (props) => {
  const Component = mapKindToComponent[props.kind];
  return (
    <div className="structured-query">
      <Component {...props} />
    </div>
  );
};

export default QueryEditor;
