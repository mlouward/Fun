class Matrix {
  mat: number[][];
  constructor(values: string) {
    const lines = values.trim().split('\n');
    this.mat = new Array();
    for (let i = 0; i < lines.length; i++) {
      this.mat.push(lines[i].split(" ").map(i => parseInt(i)));
    }
  }
  get rows(): number[][] {
    return this.mat;
  }

  get columns(): number[][] {
    // Map along a line the element at index i.
    return this.mat[0].map((_, i) => this.mat.map(row => row[i]));
  }
}

export default Matrix
