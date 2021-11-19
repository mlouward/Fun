// This stub file contains items which aren't used yet; feel free to remove this module attribute
// to enable stricter warnings.
#![allow(unused)]

use std::collections::HashMap;

pub fn can_construct_note(magazine: &[&str], note: &[&str]) -> bool {
    let mut m: HashMap<&str, i32> = HashMap::new();
    for elem in magazine {
        let entry = m.entry(elem).or_insert(0);
        *entry += 1;
    }
    for elem in note {
        let entry = m.entry(elem).or_insert(0);
        if *entry == 0 {
            return false;
        }
        *entry -= 1;
    }
    true
}
