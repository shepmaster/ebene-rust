import * as React from 'react';
import { connect } from 'react-redux';

import { Kind } from 'app/types';
import { selectAvailableLayers, selectLayerValid } from 'app/selectors';
import { State } from 'app/reducer';

import SelectKind from './SelectKind';
import { QueryEventHandlers } from './types';

interface LayerProps extends LayerOwnProps {
    id: number,
    kind: Kind,
    layers: string[],
    isValid: boolean,
    handlers: QueryEventHandlers,
}

const Layer: React.SFC<LayerProps> = ({ id, kind, name, layers, isValid, handlers }) => (
    <div>
        <SelectKind id={id} kind={kind} onKindChange={handlers.onKindChange} />
        <select value={name} onChange={e => handlers.onLayerChange(id, e.target.value)}>
            {!isValid && <option value="">Select...</option>}
            {layers.map((layer) => (<option key={layer}>{layer}</option>))}
        </select>
    </div>
);

interface LayerOwnProps {
    name: string;
}

const mapStateToProps = (state: State, props: LayerOwnProps) => ({
    isValid: selectLayerValid(state, props.name),
    layers: selectAvailableLayers(state),
});

export default connect(mapStateToProps)(Layer);
