fn sum_divisible_up_to(k: i32, bound: i32) -> i32 {
    // k + 2k + ... + k (bound / k) = k (1 + ... + bound / k)
    let l = bound / k;
    k * l * (l + 1) / 2
}

pub fn euler001() -> String {
    (sum_divisible_up_to(3, 999) + sum_divisible_up_to(5, 999) - sum_divisible_up_to(15, 999))
        .to_string()
}
