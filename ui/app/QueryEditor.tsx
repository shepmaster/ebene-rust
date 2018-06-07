import * as React from 'react';

import { Kind } from './types';
import SelectKind from './QueryEditor/SelectKind';
import Term from './QueryEditor/Term';
import Layer from './QueryEditor/Layer';

const Nothing = ({ id, kind, handlers: { onKindChange } }) => (
    <div>
        <SelectKind id={id} kind={kind} onKindChange={onKindChange} />
    </div>
);

const BinaryComponent = ({ id, kind, lhs, rhs, handlers }) => {
    const { onKindChange } = handlers;

    return (
        <div>
            <SelectKind id={id} kind={kind} onKindChange={onKindChange} />
            <div className="structured-query__child">
                <QueryEditor handlers={handlers} {...lhs} />
                <QueryEditor handlers={handlers} {...rhs} />
            </div>
        </div>
    );
};

const mapKindToComponent = {
    [Kind.Nothing]: Nothing,
    [Kind.Layer]: Layer,
    [Kind.Term]: Term,
    [Kind.Containing]: BinaryComponent,
    [Kind.ContainedIn]: BinaryComponent,
    [Kind.NotContaining]: BinaryComponent,
    [Kind.NotContainedIn]: BinaryComponent,
    [Kind.OneOf]: BinaryComponent,
    [Kind.BothOf]: BinaryComponent,
    [Kind.FollowedBy]: BinaryComponent,
};

const QueryEditor = (props) => {
    const Component = mapKindToComponent[props.kind];
    return (
        <div className="structured-query">
            <Component {...props} />
        </div>
    );
};

export default QueryEditor;
