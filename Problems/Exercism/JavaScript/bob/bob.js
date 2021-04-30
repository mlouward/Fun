//
// This is only a SKELETON file for the 'Bob' exercise. It's been provided as a
// convenience to get you started writing code faster.
//

const isSilence = message => message.trim().length === 0;
const isShouting = message => /[a-z]/i.test(message) && message === message.toUpperCase();
const isQuestion = message => message.trim().endsWith('?');
const isShoutingQuestion = message => isShouting(message) && isQuestion(message);
const isDefault = () => true;

const rules = [
  { test: isSilence, response: 'Fine. Be that way!' },
  { test: isShoutingQuestion, response: 'Calm down, I know what I\'m doing!' },
  { test: isShouting, response: 'Whoa, chill out!' },
  { test: isQuestion, response: 'Sure.' },
  { test: isDefault, response: 'Whatever.' },
];

export const hey = message => rules.find(({ test }) => test(message)).response;