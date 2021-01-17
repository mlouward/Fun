class NucleotideCount {
  static nucleotideCounts(strand: string) {
    let count_res: { [key: string]: number } = {};
    let res: { [key: string]: number } = {
      C: 0,
      G: 0,
      T: 0,
      A: 0
    };

    for (const nuc of strand) {
      if (!'CGTA'.includes(nuc))
        throw new Error('Invalid nucleotide in strand');
      res[nuc] += 1;
    }

    return res;
  }
}

export default NucleotideCount
