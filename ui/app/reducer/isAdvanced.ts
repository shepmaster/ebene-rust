import { ActionType, Action } from 'app/actions';

export type State = boolean;

export default function isAdvanced(state = false, action: Action): State {
    switch (action.type) {
        case ActionType.QueryToggle:
            return !state;
        default:
            return state;
    }
}
