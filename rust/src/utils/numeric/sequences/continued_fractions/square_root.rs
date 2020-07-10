use num::{integer::Roots, One, Zero};
use std::ops::{Add, Div, Mul, Sub};

// Iterator returning continued fraction representation of square root.
// The first number is the floor of the square root.
// If the number is a square, the iterator doesn't emit any other values.
// Iterators come in two flavors: depending on value of repeat,
// the cycle is repeated (if true) or not.
pub struct SquareRoot<T> {
    from: T,
    num: T,
    den: T,
    done: bool,
    repeat: bool,
}

impl<T> SquareRoot<T>
where
    T: Zero + One,
{
    pub fn new(from: T, repeat: bool) -> SquareRoot<T> {
        SquareRoot {
            from,
            num: T::zero(),
            den: T::one(),
            done: false,
            repeat,
        }
    }
}

impl<T> Iterator for SquareRoot<T>
where
    T: Clone + Roots,
    for<'a> &'a T: Add<Output = T> + Sub<Output = T> + Mul<Output = T> + Div<Output = T>,
{
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            None
        } else {
            let h = &(&self.from.sqrt() + &self.num) / &self.den;
            let a = &self.num - &(&self.den * &h);
            if (!self.repeat && self.den == T::one() && self.num != T::zero())
                || self.from == &a * &a
            {
                self.done = true;
            } else {
                self.num = &T::zero() - &a;
                self.den = &(&self.from - &(&a * &a)) / &self.den;
            }
            Some(h)
        }
    }
}
