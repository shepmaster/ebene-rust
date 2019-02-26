import url from 'url';

export default function primaryQuery({ q, h }) {
  const apiUrl = url.format({
    hostname: '127.0.0.1',
    port: '8080',
    pathname: '/api/search',
    query: { q, h },
  });

  return fetch(apiUrl)
    .then(r => r.json());
}
