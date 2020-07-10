use num::{FromPrimitive, One};
use std::ops::{Add, Div, Mul, Sub};

// Function produces array of size n
// a[i] contains value of phi(i + 1)
pub fn totient_array<T>(n: usize) -> Vec<T>
where
    T: One + Clone + FromPrimitive + PartialEq,
    for<'a> &'a T: Add<Output = T> + Sub<Output = T> + Mul<Output = T> + Div<Output = T>,
{
    let mut arr: Vec<T> = (1..=n).map(|x| T::from_usize(x).unwrap()).collect();
    for i in 1..n {
        let ii = T::from_usize(i + 1).unwrap();
        if arr[i] == ii {
            arr[i] = &arr[i] - &T::one();
            for j in (i + i + 1..n).step_by(i + 1) {
                arr[j] = &(&arr[j] / &ii) * &(&ii - &T::one());
            }
        }
    }
    arr
}
