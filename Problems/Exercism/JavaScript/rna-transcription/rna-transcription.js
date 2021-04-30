//
// This is only a SKELETON file for the 'RNA Transcription' exercise. It's been provided as a
// convenience to get you started writing code faster.
//
const convert = {
  'C': 'G',
  'G': 'C',
  'T': 'A',
  'A': 'U',
}

export const toRna = (input) => {
  let res = "";
  for (const base of input) {
    res += convert[base];
  }
  return res;
};
