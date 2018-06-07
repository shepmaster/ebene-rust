import { ActionType, Action } from 'app/actions';

export default function isAdvanced(state = false, action: Action) {
    switch (action.type) {
        case ActionType.QueryToggle:
            return !state;
        default:
            return state;
    }
}
