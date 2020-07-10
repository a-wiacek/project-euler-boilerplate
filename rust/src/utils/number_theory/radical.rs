use num::{FromPrimitive, One};
use std::ops::Mul;

// Function produces array of size n
// a[i] contains value of rad(i + 1)
// (rad(n) = product of the distinct prime factors of n)
pub fn radical_array<T>(n: usize) -> Vec<T>
where
    T: One + Clone + FromPrimitive + PartialEq,
    for<'a> &'a T: Mul<Output = T>,
{
    let mut arr: Vec<T> = vec![T::from_usize(1).unwrap(); n];
    for i in 1..n {
        if arr[i] == T::one() {
            for j in (i..n).step_by(i + 1) {
                arr[j] = &arr[j] * &T::from_usize(i + 1).unwrap();
            }
        }
    }
    arr
}
