import * as React from 'react';
import { connect } from 'react-redux';

import { selectTermValid, selectAvailableTerms } from 'app/selectors';
import { Kind } from 'app/types';

import SelectKind from './SelectKind';
import { QueryEventHandlers } from './types';

interface TermProps {
    id: number,
    kind: Kind,
    name: string,
    value: string,
    terms: string[],
    isValid: boolean,
    handlers: QueryEventHandlers,
}

const Term: React.SFC<TermProps> = ({ id, kind, name, value, terms, isValid, handlers }) => (
    <div>
        <SelectKind id={id} kind={kind} onKindChange={handlers.onKindChange} />
        <select value={name} onChange={e => handlers.onTermNameChange(id, e.target.value)}>
            {!isValid && <option value="">Select...</option>}
            {terms.map((term) => (<option key={term}>{term}</option>))}
        </select>
        :
        <input value={value} onChange={e => handlers.onTermValueChange(id, e.target.value)}></input>
    </div >
);

const mapStateToProps = (state, props) => ({
    isValid: selectTermValid(state, props.name),
    terms: selectAvailableTerms(state),
});

export default connect(mapStateToProps)(Term);
