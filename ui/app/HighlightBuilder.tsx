import * as React from 'react';
import { connect } from 'react-redux';
import { bindActionCreators, Dispatch, ActionCreator } from 'redux';

import { selectTreeQuery } from 'app/selectors';
import {
    highlightAdd,
    withTarget,
    withTargetIndex,
    updateKind,
    updateLayerName,
    updateTermName,
    updateTermValue,
    Action,
} from 'app/actions';
import { State } from 'app/reducer';
import { TreeQueryItem } from 'app/types';

import QueryEditor from './QueryEditor';
import { QueryEventHandlers } from './QueryEditor/types';

type HighlightOneUnconnectedProps = TreeQueryItem & HighlightOwnProps & {
    onAddHighlight: () => any,
    handlers: QueryEventHandlers,
}

// Extracting `index` to prevent passing it down further
const HighlightOneUnconnected: React.SFC<HighlightOneUnconnectedProps> = ({ index, onAddHighlight, ...props }) => (
    <div>
        <button onClick={onAddHighlight} />
        <QueryEditor {...props} />
    </div>
);

const targetMe = (action: ActionCreator<Action>, index: number) => withTarget(withTargetIndex(action, index), 'highlight');

interface HighlightOwnProps {
    index: number;
}

const mapDispatchToProps = (dispatch: Dispatch, { index }: HighlightOwnProps) => ({
    handlers: bindActionCreators({
        onKindChange: targetMe(updateKind, index),
        onLayerChange: targetMe(updateLayerName, index),
        onTermNameChange: targetMe(updateTermName, index),
        onTermValueChange: targetMe(updateTermValue, index),
    }, dispatch),
    onAddHighlight: () => dispatch(highlightAdd(index)),
});

const HighlightOne = connect(
    null,
    mapDispatchToProps
)(HighlightOneUnconnected);


interface HighlightsProps {
    highlights: TreeQueryItem[];
}

const Highlights: React.SFC<HighlightsProps> = ({ highlights }) => (
    <div>
        {highlights.map((h, i) => <HighlightOne key={i} index={i} {...h} />)}
    </div>
);

const mapStateToProps = (state: State) => ({
    highlights: state.structuredHighlights.map(selectTreeQuery),
});

export default connect(
    mapStateToProps,
    null
)(Highlights);
