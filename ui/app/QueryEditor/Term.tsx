import * as React from 'react';
import { connect } from 'react-redux';

import { selectAvailableTerms } from '../selectors';
import SelectKind from './SelectKind';

const Term = ({ id, kind, name, value, terms, handlers: { onKindChange, onTermNameChange, onTermValueChange } }) => (
    <div>
        <SelectKind id={id} kind={kind} onKindChange={onKindChange} />
        <select value={value} onChange={e => onTermNameChange(id, e.target.value)}>
            {terms.map((term) => (<option key={term}>{term}</option>))}
        </select>
        :
        <input value={value} onChange={e => onTermValueChange(id, e.target.value)}></input>
    </div >
);

const mapStateToProps = (state) => ({
    terms: selectAvailableTerms(state),
});

export default connect(mapStateToProps)(Term);
