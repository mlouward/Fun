export default class GradeSchool {
  private rosters: Map<string, string[]>;

  constructor() {
    this.rosters = new Map<string, string[]>();
  }

  public studentRoster(): Map<string, string[]> {
    return new Map([...this.rosters.entries()].sort().map(a => [a[0], a[1].slice()]));
  }

  public addStudent(name: string, grade: number): void {
    let key = grade + '';
    if (this.rosters.has(key)) {
      let names = this.rosters.get(key) as string[];
      names.push(name);
      this.rosters.set(key, names.sort());
    } else {
      this.rosters.set(key, new Array(name));
    }
  }

  studentsInGrade(grade: number): string[] {
    let key = '' + grade;
    return this.rosters.has(key) ? (this.rosters.get(key) as string[]).slice() : [];
  }
}