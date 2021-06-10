pub fn brackets_are_balanced(string: &str) -> bool {
    let mut stack: Vec<char> = Vec::new();
    for ch in string.chars() {
        // Push closing symbol on opening bracket
        match ch {
            '(' => stack.push(')'),
            '[' => stack.push(']'),
            '{' => stack.push('}'),
            // early break if error
            ']' | '}' | ')' if stack.pop() != Some(ch) => return false,
            // Ignore other characters
            _ => (),
        }
    }
    stack.is_empty()
}
