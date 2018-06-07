import * as React from 'react';
import { connect } from 'react-redux';

import { selectAvailableLayers } from '../selectors';
import SelectKind from './SelectKind';

const Layer = ({ id, kind, name, layers, handlers: { onKindChange, onLayerChange } }) => (
    <div>
        <SelectKind id={id} kind={kind} onKindChange={onKindChange} />
        <select value={name} onChange={e => onLayerChange(id, e.target.value)}>
            {layers.map((layer) => (<option key={layer}>{layer}</option>))}
        </select>
    </div>
);

const mapStateToProps = (state) => ({
    layers: selectAvailableLayers(state),
});

export default connect(mapStateToProps)(Layer);
