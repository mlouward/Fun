export default class BinarySearch {
  array: number[] | undefined;

  constructor(arr: number[]) {
    if (arr.every((val, idx, array) => !idx || array[idx - 1] <= val))
      this.array = arr;
    else
      this.array = undefined;
  }

  public indexOf(val: number): number {
    if (this.array === undefined) {
      return -1;
    }

    let top = this.array.length;
    let bot = 0;
    let idx = -1;

    do {
      if (top == bot)
        return -1;
      idx = Math.floor((top + bot) / 2);
      if (val > this.array[idx])
        bot = idx;
      else
        top = idx;
    } while (this.array[idx] != val);

    return idx;
  }
}
