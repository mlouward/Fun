fn temperature_convert(temp: f64, from: &str) -> f64 {
    if from == "c" {
        temp * 1.8 + 32.
    } else {
        temp - 32. / 1.8
    }
}

fn fibonacci(n: u32) -> u128 {
    if n == 0 {
        return 0;
    }
    if n == 1 || n == 2 {
        return 1;
    }
    let mut res: u128 = 0;
    let mut prev: u128 = 0;
    let mut cur: u128 = 1;
    for _ in 1..n {
        res = cur + prev;
        prev = cur;
        cur = res
    }
    res
}

fn main() {
    println!("{:?}", temperature_convert(38., "c"));
    println!("{:?}", temperature_convert(105., "f"));
    for x in 0..10 {
        println!("{:?}", fibonacci(x));
    }
}
