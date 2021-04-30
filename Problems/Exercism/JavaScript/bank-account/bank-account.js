//
// This is only a SKELETON file for the 'Bank Account' exercise. It's been provided as a
// convenience to get you started writing code faster.
//

export class BankAccount {
  balance;
  isopen;
  constructor() {
    this.balance = 0;
    this.isopen = false;
  }

  open() {
    if (this.isopen)
      throw new ValueError();
    else
      this.isopen = true;
  }

  close() {
    if (this.isopen) {
      this.balance = 0;
      this.isopen = false;
    }
    else
      throw new ValueError();
  }

  deposit(amount) {
    if (!(this.isopen) || amount <= 0) {
      throw new ValueError();
    }
    this.balance += amount;
  }

  withdraw(amount) {
    if (!this.isopen)
      throw new ValueError();
    else
      if (amount > this.balance || amount < 0)
        throw new ValueError();
      else {
        this.balance -= amount;
      }
  }

  get balance() {
    if (!this.isopen)
      throw new ValueError();
    else
      return this.balance;
  }

  set balance(amount) {
    this.balance = amount;
  }
}

export class ValueError extends Error {
  constructor() {
    super('Bank account error');
  }
}