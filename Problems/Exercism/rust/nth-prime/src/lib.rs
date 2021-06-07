fn is_prime(n: u32) -> bool {
    !(2..((n as f32).sqrt() as u32) + 1).any(|i| n % i == 0)
}

pub fn nth(n: u32) -> Option<u32> {
    match n {
        n if n == 0 => Some(2),
        n => (1..).filter(|c| is_prime(*c)).nth((n + 1) as usize),
    }
}
