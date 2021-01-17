export default class Luhn {
  public static valid(input: string) {
    // replace all spaces
    input = input.replace(/\s/g, '');

    // test for non-digit characters
    if (/[^\d]+/g.test(input))
      return false;

    // test for < 2 length
    if (input.length <= 1)
      return false;

    // apply the transformations
    return !(input.split('')
      .map((nb, idx) => (idx % 2 == 0 ?
        nb : parseInt(nb) * 2 > 9 ? parseInt(nb) * 2 - 9 : parseInt(nb) * 2).toString())
      .map(x => parseInt(x))
      .reduce((x, y) => x + y) % 10);
  }
}