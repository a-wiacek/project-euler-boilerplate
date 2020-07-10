use num::{One, Zero};
use std::ops::Add;

// Iterator returning continued fraction representation of Euler's number:
// e = [2; 1, 2, 1, 1, 4, 1, 1, 6, 1, ...]
pub struct E<T> {
    value: T,
    state: u8,
}

impl<T> E<T>
where
    T: Zero + One,
{
    pub fn new() -> E<T> {
        E {
            value: T::zero(),
            state: 0,
        }
    }
}

impl<T> Iterator for E<T>
where
    T: Clone + One,
    for<'a> &'a T: Add<Output = T>,
{
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            0 => {
                // Integer part of e
                self.state = 2;
                Some(&T::one() + &T::one())
            }
            3 => {
                // even numbers
                self.state = 1;
                self.value = &(&T::one() + &T::one()) + &self.value;
                Some(self.value.clone())
            }
            _ => {
                // 1
                self.state += 1;
                Some(T::one())
            }
        }
    }
}
