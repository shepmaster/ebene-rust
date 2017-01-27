export function selectQuery(state) {
  if (state.isAdvanced) {
    const { query, highlight } = state.advanced;
    return {
      q: query,
      h: highlight,
    };
  }

  const { query, within } = state.simple;

  if (query === '') {
    const structuredQuery = { Layer: { name: within } };
    return {
      q: JSON.stringify(structuredQuery),
    };
  }

  const structuredQuery = {
    Containing: [
      { Layer: { name: within } },
      { Terminal: { name: "ident", value: query } },
    ],
  };

  const structuredHighlight = [{ Terminal: { name: "ident", value: query } }];

  return {
    q: JSON.stringify(structuredQuery),
    h: JSON.stringify(structuredHighlight),
  };
}
