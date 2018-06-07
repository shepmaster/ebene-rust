import * as url from 'url';

export async function fetchLayers() {
    let resp = await fetch('/api/dev/layers');
    return resp.json();
}

export async function fetchTerms() {
    let resp = await fetch('/api/dev/terms');
    return resp.json();
}

export default async function primaryQuery({ q, h }) {
    const apiUrl = url.format({
        pathname: '/api/search',
        query: { q, h },
    });

    let resp = await fetch(apiUrl);
    return resp.json();
}
