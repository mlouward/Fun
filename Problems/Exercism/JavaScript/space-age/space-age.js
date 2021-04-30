//
// This is only a SKELETON file for the 'Space Age' exercise. It's been provided as a
// convenience to get you started writing code faster.
//
const EARTH_SEC = 31557600;

function round(number) {
  return Math.round((number + Number.EPSILON) * 100) / 100;
}

export const age = (planet, secs) => {
  switch (planet) {
    case "mercury":
      return round(secs / EARTH_SEC / 0.2408467);
    case "venus":
      return round(secs / EARTH_SEC / 0.61519726);
    case "earth":
      return round(secs / EARTH_SEC / 1);
    case "mars":
      return round(secs / EARTH_SEC / 1.8808158);
    case "jupiter":
      return round(secs / EARTH_SEC / 11.862615);
    case "saturn":
      return round(secs / EARTH_SEC / 29.447498);
    case "uranus":
      return round(secs / EARTH_SEC / 84.016846);
    case "neptune":
      return round(secs / EARTH_SEC / 164.79132);
  }
};
