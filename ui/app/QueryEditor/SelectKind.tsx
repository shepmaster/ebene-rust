import * as React from 'react';

import { Kind } from '../types';

const SelectKind = ({ id, kind, onKindChange }) => {
    const options = Object.values(Kind).map((name, i) => (
        <option key={i} value={name}>{name}</option>
    ));

    return (
        <select value={kind} onChange={e => onKindChange(id, e.target.value)}>
            {options}
        </select>
    );
};

export default SelectKind;
