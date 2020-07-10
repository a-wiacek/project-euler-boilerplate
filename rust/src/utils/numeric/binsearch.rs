use num::One;
use std::ops::{Add, Div, Sub};

// Find the largest number in interval [begin, end] satisfying function f.
// It is assumed that there exists x such that
// f is satisfied on interval [begin, x] and not satisfied on interval (x, end]
pub fn binsearch<T, F>(begin: &T, end: &T, f: F) -> T
where
    F: Fn(T) -> bool,
    T: Clone + One + PartialOrd,
    for<'a> &'a T: Add<Output = T> + Sub<Output = T> + Div<Output = T>,
{
    let mut _begin = begin.clone();
    let mut _end = end.clone();
    let _2 = &(&T::one() + &T::one());
    while _begin < _end {
        let _mid = &(&(&_begin + &_end) + &T::one()) / _2;
        if f(_mid.clone()) {
            _begin = _mid;
        } else {
            _end = &_mid - &T::one();
        }
    }
    _begin
}
