use num::{One, Zero};
use std::ops::{Mul, Sub};

// It is assumed that n >= 0
pub fn factorial<T>(n: &T) -> T
where
    T: Zero + One + PartialEq,
    for<'a> &'a T: Sub<Output = T> + Mul<Output = T>,
{
    if n.is_one() || n.is_zero() {
        T::one()
    } else {
        n * &(factorial(&(n - &T::one())))
    }
}
