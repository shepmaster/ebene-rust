export default function primaryQuery({ q, h }) {
  var url = `http://127.0.0.1:8080/api/search?`;

  if (q) {
    url += `q=${q}`;
  }

  if (h) {
    url += `&h=${h}`;
  }

  return fetch(url)
    .then(r => r.json());
}
