//
// This is only a SKELETON file for the 'Matrix' exercise. It's been provided as a
// convenience to get you started writing code faster.
//

export class Matrix {
  constructor(input) {
    const lines = input.trim().split('\n');
    let a = lines[0].split(" ");
    this.mat = new Array();
    for (let i = 0; i < lines.length; i++) {
      this.mat.push(lines[i].split(" ").map(i => parseInt(i)));
    }
  }

  get rows() {
    return this.mat;
  }

  get columns() {
    return this.mat[0].map((_, i) => this.mat.map(row => row[i]));
  }
}