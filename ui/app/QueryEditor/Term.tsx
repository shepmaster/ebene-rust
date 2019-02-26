import * as React from 'react';
import { connect } from 'react-redux';

import { selectTermValid, selectAvailableTerms } from 'app/selectors';
import { Kind } from 'app/types';

import SelectKind from './SelectKind';
import { QueryEventHandlers } from './types';
import { State } from '../reducer';

interface TermProps extends TermOwnProps {
    id: number,
    kind: Kind,
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

interface TermOwnProps {
    name: string,
}

const mapStateToProps = (state: State, props: TermOwnProps) => ({
    isValid: selectTermValid(state, props.name),
    terms: selectAvailableTerms(state),
});

export default connect(mapStateToProps)(Term);
