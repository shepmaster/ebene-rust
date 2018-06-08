import * as React from 'react';
import { connect } from 'react-redux';
import { bindActionCreators } from 'redux';

import { selectTreeQuery } from 'app/selectors';
import { updateKind, updateLayerName, updateTermName, updateTermValue, retarget } from 'app/actions';

import QueryEditor from './QueryEditor';
import { State } from './reducer';

const mapStateToProps = (state: State) => selectTreeQuery(state.structuredQuery);

const targetMe = (action) => retarget(action, 'query');

const mapDispatchToProps = (dispatch) => ({
    handlers: bindActionCreators({
        onKindChange: targetMe(updateKind),
        onLayerChange: targetMe(updateLayerName),
        onTermNameChange: targetMe(updateTermName),
        onTermValueChange: targetMe(updateTermValue),
    }, dispatch)
});

export default connect(
    mapStateToProps,
    mapDispatchToProps
)(QueryEditor);
