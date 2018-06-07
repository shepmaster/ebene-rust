import { ActionType, Action } from 'app/actions';

export default function results(state = [], action: Action) {
    switch (action.type) {
        case ActionType.QueryResults:
            if (!action.error) {
                return action.payload.results;
            } else {
                return state;
            }
        default:
            return state;
    }
}
