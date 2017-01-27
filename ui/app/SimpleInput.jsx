import React from 'react';
import { connect } from 'react-redux';

import { updateQuery, updateWithin } from './actions';

const SimpleInput = ({ query, within, onQueryChange, onContainerChange }) => (
  <div>
    <input value={query} onChange={e => onQueryChange(e.target.value)}></input>
    <select value={within} onChange={e => onContainerChange(e.target.value)}>
      <option value="function">Function</option>
      <option value="enum">Enum</option>
      <option value="struct">Struct</option>
    </select>
  </div>
);

const mapStateToProps = ({ simple: { query, within } }) => ({
  query,
  within,
});

const mapDispatchToProps = (dispatch) => ({
  onQueryChange: q => dispatch(updateQuery(q)),
  onContainerChange: w => dispatch(updateWithin(w)),
});

export default connect(
  mapStateToProps,
  mapDispatchToProps
)(SimpleInput);
