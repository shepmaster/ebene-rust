import * as React from 'react';

import { Kind } from 'app/types';

import { QueryEventHandlers } from './types';
import SelectKind from './SelectKind';

interface NothingProps {
    id: number,
    kind: Kind,
    handlers: QueryEventHandlers;
}

const Nothing: React.SFC<NothingProps> = ({ id, kind, handlers }) => (
    <div>
        <SelectKind id={id} kind={kind} onKindChange={handlers.onKindChange} />
    </div>
);

export default Nothing;
