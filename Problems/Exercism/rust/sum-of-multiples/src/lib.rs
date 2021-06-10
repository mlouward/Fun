pub fn sum_of_multiples(limit: u32, factors: &[u32]) -> u32 {
    if limit <= 1 {
        return 0;
    }
    let mut res = Vec::new();
    for factor in factors {
        // Ignore 0 as a factor
        if factor == &0 {
            continue;
        }
        res.append(&mut (1..limit).filter(|x| x % factor == 0).collect::<Vec<u32>>());
    }
    // remove duplicates and sum
    res.sort();
    res.dedup();
    res.iter().sum()
}
