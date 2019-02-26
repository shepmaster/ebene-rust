import { ActionType, Action } from 'app/actions';

export interface State {
    query: string;
    highlight: string;
}

const initialState: State = {
    query: '{"Layer": {"name": "function"}}',
    highlight: '[{"Term": {"name": "ident", "value": "pm"}}]',
};

export default function advanced(state = initialState, action: Action): State {
    switch (action.type) {
        case ActionType.AdvancedQueryUpdate: {
            const { query } = action.payload;
            return { ...state, query };
        }
        case ActionType.AdvancedHighlightUpdate: {
            const { highlight } = action.payload;
            return { ...state, highlight };
        }
        default:
            return state;
    }
};
