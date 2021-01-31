export default class Clock {
  hours: number;
  minutes: number;
  constructor(hours: number, minutes: number = 0) {
    let rolloverHours: number = 1;
    if (minutes < 0) {
      hours = hours - 1 - (Math.floor(-minutes / 60 % 24));
      minutes = 60 - (-minutes % 60)
    }
    while (hours < 0) {
      hours += 24;
      rolloverHours += 1;
    }
    if (minutes < 0)
      minutes = rolloverHours * 24 * 60 + minutes;

    const overflow = Math.floor(minutes / 60);
    this.minutes = minutes % 60;
    this.hours = (hours + overflow) % 24;
  }

  public toString(): string {
    return `${this.hours.toString().padStart(2, '0')}:${this.minutes.toString().padStart(2, '0')}`;
  }

  public plus(addedMinutes: number): Clock {
    return new Clock(this.hours, this.minutes + addedMinutes);
  }

  public minus(subbedMinutes: number): Clock {
    return this.plus(-subbedMinutes);
  }

  public equals(other: Clock): boolean {
    return this.toString() === other.toString();
  }
}