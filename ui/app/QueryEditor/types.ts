import { Kind } from "app/types";

export interface QueryEventHandlers {
    onKindChange: (id: number, kind: Kind) => any;
    onLayerChange: (id: number, layer: string) => any;
    onTermNameChange: (id: number, layer: string) => any;
    onTermValueChange: (id: number, layer: string) => any;
}
