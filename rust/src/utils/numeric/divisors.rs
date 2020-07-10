use num::{Integer, One, Zero};
use std::ops::{Add, Div, Mul, Sub};

pub fn divisors<T>(n: T) -> Vec<T>
where
    T: Integer + Zero + One + Clone,
    for<'a> &'a T: Add<Output = T> + Sub<Output = T> + Mul<Output = T> + Div<Output = T>,
{
    if &n == &T::zero() {
        panic!("Divisors function called for 0")
    } else if &n < &T::zero() {
        let mut divisors = divisors(&T::zero() - &n);
        let negative_divisors: Vec<T> = divisors.iter().map(|d| &T::zero() - d).collect();
        divisors.extend(negative_divisors);
        divisors
    } else {
        let mut lsq_divisors = Vec::new();
        let mut d = T::one();
        while &d * &d < n {
            if n.is_multiple_of(&d) {
                lsq_divisors.push(d.clone());
            }
            d = d + T::one();
        }
        let gsq_divisors: Vec<T> = lsq_divisors.iter().rev().map(|d| &n / d).collect();
        if &(&d * &d) == &n {
            [lsq_divisors, vec![d], gsq_divisors].concat()
        } else {
            [lsq_divisors, gsq_divisors].concat()
        }
    }
}
