import * as React from 'react';

import { Kind, TreeQueryItem } from 'app/types';

import QueryEditor from '../QueryEditor';
import { QueryEventHandlers } from './types';
import SelectKind from './SelectKind';

interface BinaryComponentProps {
    id: number,
    kind: Kind,
    lhs: TreeQueryItem,
    rhs: TreeQueryItem,
    handlers: QueryEventHandlers;
}

const BinaryComponent: React.SFC<BinaryComponentProps> = ({ id, kind, lhs, rhs, handlers }) => (
    <div>
        <SelectKind id={id} kind={kind} onKindChange={handlers.onKindChange} />
        <div className="structured-query__child">
            <QueryEditor handlers={handlers} {...lhs} />
            <QueryEditor handlers={handlers} {...rhs} />
        </div>
    </div>
);

export default BinaryComponent;
