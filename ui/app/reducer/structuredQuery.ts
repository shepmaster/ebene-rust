import { Kind, makeNothing, FlatQueryItems, isBinaryKind, FlatBinaryItem, isFlatBinaryItem } from 'app/types';
import { ActionType, Action } from 'app/actions';

import { initial, forTarget } from './higherOrder';

export type State = FlatQueryItems;

export function rawReducer(state: State, action: Action): State {
    switch (action.type) {
        case ActionType.StructuredQueryKindUpdate: {
            const { kind, id } = action.payload;

            if (isBinaryKind(kind)) {
                const old = state[id];

                if (isFlatBinaryItem(old)) {
                    const { lhs, rhs } = old;
                    const newx: FlatBinaryItem = { kind, lhs, rhs } as FlatBinaryItem; // xxx

                    return {
                        ...state,
                        [id]: newx,
                    }
                } else {
                    // This isn't as efficient as it could be. Every time we
                    // change to a binary type, we create brand new children
                    // nodes and never recycle them.
                    const lhs = Object.keys(state).length;
                    const rhs = lhs + 1;
                    const newx: FlatBinaryItem = { kind, lhs, rhs } as FlatBinaryItem; // xxx

                    return {
                        ...state,
                        [id]: newx,
                        [lhs]: makeNothing(),
                        [rhs]: makeNothing(),
                    }
                }
            } else if (kind === Kind.Layer) {
                return {
                    ...state,
                    [id]: { kind, name: '' }
                }
            } else if (kind === Kind.Nothing) {
                return {
                    ...state,
                    [id]: { kind }
                }
            } else if (kind === Kind.Term) {
                return {
                    ...state,
                    [id]: { kind, name: '', value: '' }
                }
            }

            const unreachable: never = kind;
            return unreachable;
        }
        case ActionType.LayerNameUpdate: {
            const { name, id } = action.payload;
            const old = state[id];

            if (old.kind === Kind.Layer) {
                return { ...state, [id]: { ...old, name } };
            } else {
                return state;
            }
        }
        case ActionType.TermNameUpdate: {
            const { name, id } = action.payload;
            const old = state[id];

            if (old.kind === Kind.Term) {
                return { ...state, [id]: { ...old, name } };
            } else {
                return state;
            }
        }
        case ActionType.TermValueUpdate: {
            const { value, id } = action.payload;
            const old = state[id];

            if (old.kind === Kind.Term) {
                return { ...state, [id]: { ...old, value } };
            } else {
                return state;
            }
        }
        default:
            return state;
    }
}

const initialStructuredQuery: State = {
    0: { kind: Kind.Containing, lhs: 1, rhs: 2 },
    1: { kind: Kind.Layer, name: 'function' },
    2: { kind: Kind.Term, name: 'ident', value: 'pm' },
};

const targetedReducer = forTarget(rawReducer, 'query');
export default initial(targetedReducer, initialStructuredQuery);
