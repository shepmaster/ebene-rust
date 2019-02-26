import { isWithTarget, isWithTargetIndex } from 'app/actions';

export const forTarget = <S, A>(reducer: (s: S, a: A) => S, target: string) =>
    (state: S, action: A) =>
        isWithTarget(action) && action.meta.target === target ? reducer(state, action) : state;

export const forTargetIndex = <S, A>(reducer: (state: S, action: A) => S) =>
    (state: S[], action: A) => {
        if (isWithTargetIndex(action) && action.meta.targetIndex < state.length) {
            const index = action.meta.targetIndex;
            const result = reducer(state[index], action);
            const newState = state.slice();
            newState.splice(index, 1, result);
            return newState;
        } else {
            return state;
        }
    }

export const initial = <S, A>(reducer: (s: S, a: A) => S, initialState: S) =>
    (state: S | undefined, action: A) =>
        reducer(state || initialState, action);
