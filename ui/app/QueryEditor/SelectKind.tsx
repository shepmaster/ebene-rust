import * as React from 'react';

import { Kind } from '../types';
import { QueryEventHandlers } from './types';

interface SelectKindProps {
    id: number,
    kind: Kind,
    onKindChange: QueryEventHandlers['onKindChange'];
}

const SelectKind: React.SFC<SelectKindProps> = ({ id, kind, onKindChange }) => (
    <select value={kind} onChange={e => onKindChange(id, e.target.value as Kind)}>
        {Object.values(Kind).map(name => (
            <option key={name} value={name}>{name}</option>
        ))}
    </select>
);

export default SelectKind;
