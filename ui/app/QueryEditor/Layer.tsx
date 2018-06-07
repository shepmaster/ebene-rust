import * as React from 'react';
import { connect } from 'react-redux';

import { selectAvailableLayers, selectLayerValid } from '../selectors';
import SelectKind from './SelectKind';

const Layer = ({ id, kind, name, layers, isValid, handlers: { onKindChange, onLayerChange } }) => (
    <div>
        <SelectKind id={id} kind={kind} onKindChange={onKindChange} />
        <select value={name} onChange={e => onLayerChange(id, e.target.value)}>
            {!isValid && <option value="">Select...</option>}
            {layers.map((layer) => (<option key={layer}>{layer}</option>))}
        </select>
    </div>
);

const mapStateToProps = (state, props) => ({
    isValid: selectLayerValid(state, props.name),
    layers: selectAvailableLayers(state),
});

export default connect(mapStateToProps)(Layer);
