export default class Raindrops {
  public convert(value: number): string {
    let res: string = "";

    if (!(value % 3))
      res += "Pling";
    if (!(value % 5))
      res += "Plang";
    if (!(value % 7))
      res += "Plong";
    if (!res)
      return value.toString();

    return res;
  }
}