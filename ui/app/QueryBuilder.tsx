import * as React from 'react';
import { connect } from 'react-redux';
import { bindActionCreators, ActionCreator, Dispatch } from 'redux';

import { selectTreeQuery } from 'app/selectors';
import { updateKind, updateLayerName, updateTermName, updateTermValue, withTarget, Action } from 'app/actions';

import QueryEditor from './QueryEditor';
import { State } from './reducer';

const mapStateToProps = (state: State) => selectTreeQuery(state.structuredQuery);

const targetMe = (action: ActionCreator<Action>) => withTarget(action, 'query');

const mapDispatchToProps = (dispatch: Dispatch) => ({
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
