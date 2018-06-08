import { Kind, makeNothing, FlatQueryItems } from 'app/types';
import { ActionType, Action } from 'app/actions';

import { initial, forTarget, forTargetIndex } from './higherOrder';

export function rawReducer(state, action: Action) {
    switch (action.type) {
        case ActionType.StructuredQueryKindUpdate: {
            const { kind, id } = action.payload;
            const old = state[id];

            if (old.lhs === undefined || old.rhs === undefined) {
                const lhs = Object.keys(state).length;
                const rhs = lhs + 1;
                return {
                    ...state,
                    [lhs]: makeNothing(),
                    [rhs]: makeNothing(),
                    [id]: { ...old, kind, lhs, rhs }
                };
            } else {
                return { ...state, [id]: { ...old, kind } };
            }
        }
        case ActionType.LayerNameUpdate: {
            const { name, id } = action.payload;
            const old = state[id];

            return { ...state, [id]: { ...old, name } };
        }
        case ActionType.TermNameUpdate: {
            const { name, id } = action.payload;
            const old = state[id];

            return { ...state, [id]: { ...old, name } };
        }
        case ActionType.TermValueUpdate: {
            const { value, id } = action.payload;
            const old = state[id];

            return { ...state, [id]: { ...old, value } };
        }
        default:
            return state;
    }
}

export type State = FlatQueryItems;

const initialStructuredQuery: State = {
    0: { kind: Kind.Containing, lhs: 1, rhs: 2 },
    1: { kind: Kind.Layer, name: 'function' },
    2: { kind: Kind.Term, name: 'ident', value: 'pm' },
};

export default initial(forTarget(rawReducer, 'query'), initialStructuredQuery);
