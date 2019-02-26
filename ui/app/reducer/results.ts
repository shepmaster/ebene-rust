import { ActionType, Action } from 'app/actions';
import { QueryResult } from 'app/types';

export type State = QueryResult[];

const initialState: State = []

export default function results(state = initialState, action: Action): State {
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
