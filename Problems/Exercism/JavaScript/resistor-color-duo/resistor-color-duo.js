//
// This is only a SKELETON file for the 'Resistor Color Duo' exercise. It's been provided as a
// convenience to get you started writing code faster.
//

export const decodedValue = (colorsArray) => {
    return 10 * COLORS.findIndex(a => a == colorsArray[0]) + COLORS.findIndex(a => a == colorsArray[1]);
};

export const COLORS = ['black', 'brown', 'red', 'orange', 'yellow', 'green', 'blue', 'violet', 'grey', 'white'];