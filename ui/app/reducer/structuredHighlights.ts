import { ActionType, Action } from 'app/actions';
import { Kind, FlatQueryItems, makeNothing } from 'app/types';

import { forTarget, forTargetIndex } from './higherOrder';
import { rawReducer } from './structuredQuery';

const structuredHighlightReducer = forTarget(forTargetIndex(rawReducer), 'highlight');

const initialStructuredHighlight: FlatQueryItems[] = [{
    0: { kind: Kind.Term, name: 'ident', value: 'pm' },
}];

export default function structuredHighlightsReducer(state = initialStructuredHighlight, action: Action) {
    switch (action.type) {
        case ActionType.HighlightAdd: {
            const { index } = action.payload;
            const newState = state.slice();
            newState.splice(index + 1, 0, { 0: makeNothing() });
            return newState;
        }
        default:
            return structuredHighlightReducer(state, action);
    }
}
