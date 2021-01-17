export default class Robot {
  public name: string;
  private usedNames: string[] = [];
  private static allNames: string[] = "AZERTYUIOPQSDFGHJKLMWXCVBN"
    .split("")
    // Create ["AA", "AZ", "AE", ...]
    .flatMap((l, _, arr) => { return arr.map(r => l + r) })
    // foreach pair of letters, append 3 digits (between 0 and 1000) to create "AA000"...
    .flatMap(l => [...Array(1000).keys()]
      .map(digit => digit < 10 ? l + "00" + digit : digit < 100 ? l + "0" + digit : l + digit));

  constructor() {
    this.name = "";
    this.resetName();
  }

  public resetName(): void {
    let newName = Robot.allNames[Math.floor(Math.random() * Robot.allNames.length)];
    if (this.name != undefined) {
      // if not in constructor call ...
      this.usedNames.push(this.name);
      // assure we have not used that name for this instance before
      while (this.usedNames.indexOf(newName) > -1) {
        newName = Robot.allNames[Math.floor(Math.random() * Robot.allNames.length)];
      }
    }
    this.name = newName;
  }

  public static releaseNames(): void {
    Robot.allNames = "AZERTYUIOPQSDFGHJKLMWXCVBN"
      .split("")
      // Create ["AA", "AZ", "AE", ...]
      .flatMap((l, _, arr) => { return arr.map(r => l + r) })
      // foreach pair of letters, append 3 digits (between 0 and 1000) to create "AA000"...
      .flatMap(l => [...Array(1000).keys()]
        .map(digit => digit < 10 ? l + "00" + digit : digit < 100 ? l + "0" + digit : l + digit));
  }
}
