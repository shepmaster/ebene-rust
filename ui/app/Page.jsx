import React from 'react';
import { connect } from 'react-redux';

import SimpleInput from './SimpleInput';
import AdvancedInput from './AdvancedInput';
import ResultList from './ResultList';
import { toggleAdvanced } from './actions';

const Input = ({ isAdvanced }) => (
  isAdvanced ? <AdvancedInput /> : <SimpleInput />
);

const Page = ({ isAdvanced, results, toggleAdvanced }) => (
  <div>
    <button onClick={toggleAdvanced}>Mode</button>
    <Input isAdvanced={isAdvanced} />
    <ResultList results={results} />
  </div>
);

const mapStateToProps = ({ isAdvanced, results }) => ({
  isAdvanced,
  results,
});

const mapDispatchToProps = (dispatch) => ({
  toggleAdvanced: () => dispatch(toggleAdvanced()),
});

export default connect(
  mapStateToProps,
  mapDispatchToProps
)(Page);
