import * as React from 'react';

import { Kind, TreeQueryItem } from 'app/types';

import { QueryEventHandlers } from './QueryEditor/types';
import BinaryComponent from './QueryEditor/BinaryComponent';
import Layer from './QueryEditor/Layer';
import Nothing from './QueryEditor/Nothing';
import SelectKind from './QueryEditor/SelectKind';
import Term from './QueryEditor/Term';

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

interface QueryEditorProps {
    handlers: QueryEventHandlers,
}

const QueryEditor: React.SFC<QueryEditorProps & TreeQueryItem> = (props) => {
    const Component = mapKindToComponent[props.kind];
    return (
        <div className="structured-query">
            <Component {...props} />
        </div>
    );
};

export default QueryEditor;
