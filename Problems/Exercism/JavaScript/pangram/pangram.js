//
// This is only a SKELETON file for the 'Pangram' exercise. It's been provided as a
// convenience to get you started writing code faster.
//

export const isPangram = (input) => {
  const verif = Array(26).fill(false);
  for (const letter of input) {
    if (letter != " ") {
      verif[letter.toLowerCase().charCodeAt(0) - 'a'.charCodeAt(0)] = true;
    }
  }
  return verif.every(x => x === true);
};