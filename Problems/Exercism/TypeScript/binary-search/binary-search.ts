export default class BinarySearch {
  array: number[] | undefined;

  constructor(arr: number[]) {
    let tmp = arr[0];
    for (let i = 1; i < arr.length; i++) {
      if (arr[i] < tmp) {
        this.array = undefined;
        return;
      }
    }
    this.array = arr;
  }

  public indexOf(val: number) {
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
