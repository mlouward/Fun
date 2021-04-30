function transform(old_data: { [key: number]: string[] }): { [key: string]: number } {
  let new_data: { [key: string]: number } = {};
  for (let key in old_data) {
    for (let letter of old_data[key]) {
      if (new_data[letter])
        new_data[letter] += parseInt(key);
      else
        new_data[letter.toLowerCase()] = parseInt(key);
    }
  }
  return new_data;
}

export default transform