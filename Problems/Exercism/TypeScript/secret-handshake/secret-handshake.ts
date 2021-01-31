export default class HandShake {
  bin: string[];
  constructor(nb: number) {
    this.bin = nb.toString(2).split('');
  }

  public commands(): string[] {
    let res: string[] = new Array();
    // reverse to start from least significant bit
    this.bin.reverse().filter((val, idx) => {
      if (val == '1') {
        switch (idx) {
          case 0:
            res.push('wink');
            break;
          case 1:
            res.push('double blink');
            break;
          case 2:
            res.push('close your eyes');
            break;
          case 3:
            res.push('jump');
            break;
          case 4:
            res.reverse();
            break;

          default:
            break;
        }
      }
    })
    return res;
  }
}