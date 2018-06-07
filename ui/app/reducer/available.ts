import { ActionType, Action } from 'app/actions';

const initialState = {
    layers: [],
    terms: [],
};

export default function available(state = initialState, action: Action) {
    switch (action.type) {
        case ActionType.AvailableLayersUpdate: {
            return { ...state, layers: action.payload.layers };
        }
        case ActionType.AvailableTermsUpdate: {
            return { ...state, terms: action.payload.terms };
        }
        default:
            return state;
    }
}
