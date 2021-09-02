pub fn production_rate_per_hour(speed: u8) -> f64 {
    (speed as f64)
        * 221_f64
        * match speed {
            s if s <= 4 => 1_f64,
            s if s <= 8 => 0.9_f64,
            _ => 0.77_f64,
        }
}

pub fn working_items_per_minute(speed: u8) -> u32 {
    (production_rate_per_hour(speed) / 60_f64) as u32
}
