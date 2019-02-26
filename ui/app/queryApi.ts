import * as url from 'url';

import { QueryResult } from "./types";

interface ApiLayersResponse {
    layers: string[];
}

export async function fetchLayers(): Promise<ApiLayersResponse> {
    let resp = await fetch('/api/dev/layers');
    return resp.json();
}

interface ApiTermsResponse {
    terms: string[];
}

export async function fetchTerms(): Promise<ApiTermsResponse> {
    let resp = await fetch('/api/dev/terms');
    return resp.json();
}

interface QueryArgs {
    q?: string;
    h?: string;
}

interface ApiQueryResponse {
    results: QueryResult[];
}

export default async function primaryQuery({ q, h }: QueryArgs): Promise<ApiQueryResponse> {
    const apiUrl = url.format({
        pathname: '/api/search',
        query: { q, h },
    });

    let resp = await fetch(apiUrl);
    return resp.json();
}
