import { ActionType, Action } from 'app/actions';

export interface State {
    layers: string[];
    terms: string[];
}

const initialState: State = {
    layers: [],
    terms: [],
};

export default function available(state = initialState, action: Action): State {
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
