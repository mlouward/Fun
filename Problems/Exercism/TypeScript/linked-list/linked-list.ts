class Node<T> {
  previous: Node<T> | null;
  next: Node<T> | null;
  value: T | null;

  constructor(value?: T) {
    this.previous = null;
    this.next = null;
    this.value = value || null;
  }
}

export class LinkedList<T> {
  first: Node<T>;
  last: Node<T>;
  _count: number = 0;

  constructor() {
    this.first = new Node();
    this.last = new Node();
  }

  push(value: T): void {
    const pushed: Node<T> = new Node<T>(value)

    if (this._count === 0) {
      this.first = pushed;
    } else {
      this.last.next = pushed;
      pushed.previous = this.last;
    }
    this.last = pushed;
    this._count++;
  }

  pop(): T {
    if (this.last === null) {
      return null;
    }

    const popped: Node<T> = this.last;
    if (popped.previous) {
      popped.previous.next = popped.next;
    }
    if (popped.next) {
      popped.next.previous = popped.previous;
    }
    this.last = popped.previous;
    this._count--;
    return popped.value;
  }

  shift(): T {
    if (this.last === null) {
      return null;
    }

    const shifted: Node<T> = this.first;
    if (shifted.previous) {
      shifted.previous.next = shifted.next;
    }
    if (shifted.next) {
      shifted.next.previous = shifted.previous;
    }
    this.last = shifted.previous;
    this._count--;
    return shifted.value;
  }

  unshift(value: T) {
    const pushed: Node<T> = new Node<T>(value)

    if (this._count === 0) {
      this.first = pushed;
    } else {
      this.first.previous = pushed;
      pushed.next = this.first.previous;
    }
    this.first = pushed;
    this._count++;
  }

  delete(value: T) {
    let tmp = this.first;
    while (tmp !== null) {
      if (tmp.value === value) {
        tmp.previous.next = tmp.next;
        tmp.next.previous = tmp.previous;
        this._count--;
        return;
      }
      tmp = tmp.next;
    }
  }

  count(): number {
    return this._count;
  }
}
