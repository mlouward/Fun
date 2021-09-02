#![allow(dead_code, unused_variables)]
#[derive(Debug)]
struct Rectangle {
    width: u32,
    height: u32,
}

impl Rectangle {
    fn area(&self) -> u32 {
        self.width * self.height
    }
}

fn main() {
    let rect1 = Rectangle {
        width: 30,
        height: 50,
    };

    println!(
        "The area of the rectangle is {} square pixels.",
        rect1.area()
    );

    println!("{:#?}", rect1);

    let home = IpAddr::V4(127, 0, 0, 1);
    let loopback = IpAddr::V6(String::from("::1"));

    if let Some(4) = Some(3) {
        println!("three");
    }
}

enum IpAddr {
    V4(u8, u8, u8, u8),
    V6(String),
}

fn route(ip: IpAddr) {}
