export const countOccurrences = <T extends PropertyKey>(
  array: Array<T>
): Record<T, number> => {
  const occurrencesGroups = array.reduce((acc, curr) => {
    if (curr in acc) {
      acc[curr]++;
    } else {
      acc[curr] = 1;
    }
    return acc;
  }, {} as Record<T, number>);

  return occurrencesGroups;
};
