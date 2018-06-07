export const initial = (reducer, initialState) => (state = initialState, action) => (
    reducer(state, action)
);

export const forTarget = (reducer, expectedTarget: string) => (state, action) => {
    if (action.payload && action.payload.target === expectedTarget) {
        return reducer(state, action);
    } else {
        return state;
    }
};

export const forTargetIndex = (reducer) => (state, action) => {
    const index = action.payload.targetIndex;
    const result = reducer(state[index], action);
    const newState = state.slice();
    newState.splice(index, 1, result);
    return newState;
};
