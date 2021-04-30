function isLeapYear(year: number): boolean | number {
  return !(year % 4) && (year % 100 || !(year % 400));
}

export default isLeapYear
