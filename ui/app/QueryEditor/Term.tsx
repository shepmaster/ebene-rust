import * as React from 'react';
import { connect } from 'react-redux';

import { selectTermValid, selectAvailableTerms } from '../selectors';
import SelectKind from './SelectKind';

const Term = ({ id, kind, name, value, terms, isValid, handlers: { onKindChange, onTermNameChange, onTermValueChange, } }) => (
    <div>
        <SelectKind id={id} kind={kind} onKindChange={onKindChange} />
        <select value={name} onChange={e => onTermNameChange(id, e.target.value)}>
            {!isValid && <option value="">Select...</option>}
            {terms.map((term) => (<option key={term}>{term}</option>))}
        </select>
        :
        <input value={value} onChange={e => onTermValueChange(id, e.target.value)}></input>
    </div >
);

const mapStateToProps = (state, props) => ({
    isValid: selectTermValid(state, props.name),
    terms: selectAvailableTerms(state),
});

export default connect(mapStateToProps)(Term);
