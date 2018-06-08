import * as React from 'react';
import { connect } from 'react-redux';

import { QueryResult } from 'app/types';
import { State } from 'app/reducer';
import { toggleAdvanced } from 'app/actions';

import AdvancedInput from './AdvancedInput';
import HighlightBuilder from './HighlightBuilder';
import QueryBuilder from './QueryBuilder';
import ResultList from './ResultList';

interface InputProps {
    isAdvanced: boolean;
}

const Input: React.SFC<InputProps> = ({ isAdvanced }) => (
    isAdvanced ? <AdvancedInput /> : <div><QueryBuilder /><HighlightBuilder /></div>
);

interface PageProps {
    isAdvanced: boolean;
    results: QueryResult[];
    toggleAdvanced: () => any;
}

const Page: React.SFC<PageProps> = ({ isAdvanced, results, toggleAdvanced }) => (
    <div>
        <button onClick={toggleAdvanced}>Mode</button>
        <Input isAdvanced={isAdvanced} />
        <ResultList results={results} />
    </div>
);

const mapStateToProps = ({ isAdvanced, results }: State) => ({
    isAdvanced,
    results,
});

const mapDispatchToProps = {
    toggleAdvanced,
};

export default connect(
    mapStateToProps,
    mapDispatchToProps,
)(Page);
