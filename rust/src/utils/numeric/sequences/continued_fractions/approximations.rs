use num::{rational::Ratio, One, Zero};
use std::ops::{Add, Mul};

// Approximations of continued fraction
pub struct Approximations<'a, T> {
    iterator: Box<dyn Iterator<Item = T> + 'a>,
    num_prev_prev: T,
    num_prev: T,
    den_prev_prev: T,
    den_prev: T,
}

impl<'a, T> Approximations<'a, T>
where
    T: Zero + One,
{
    pub fn new(iterator: impl Iterator<Item = T> + 'a) -> Approximations<'a, T> {
        Approximations {
            iterator: Box::new(iterator),
            num_prev_prev: T::zero(),
            num_prev: T::one(),
            den_prev_prev: T::one(),
            den_prev: T::zero(),
        }
    }
}

impl<'a, T> Iterator for Approximations<'a, T>
where
    T: Clone + One,
    for<'b> &'b T: Add<Output = T> + Mul<Output = T>,
{
    type Item = Ratio<T>;
    fn next(&mut self) -> Option<Self::Item> {
        self.iterator.next().map(|a| {
            let num_new = &(&a * &self.num_prev) + &self.num_prev_prev;
            let den_new = &(&a * &self.den_prev) + &self.den_prev_prev;
            self.num_prev_prev = self.num_prev.clone();
            self.num_prev = num_new;
            self.den_prev_prev = self.den_prev.clone();
            self.den_prev = den_new;
            Ratio::new_raw(self.num_prev.clone(), self.den_prev.clone())
        })
    }
}
