use num::{Integer, One, ToPrimitive, Zero};
use std::convert::TryFrom;
use std::iter::Sum;
use std::ops::{Add, Mul, Sub};

pub fn undigits<T>(digits: &Vec<T>) -> T
where
    T: Zero + One,
    for<'a> &'a T: Add<Output = T> + Mul<Output = T>,
{
    let _2 = &T::one() + &T::one();
    let _10 = &(&(&_2 * &_2) * &_2) + &_2;
    digits.iter().fold(T::zero(), |a, b| &(&_10 * &a) + b)
}

pub trait Digits<T> {
    fn digits_in_radix(&self, radix: usize) -> Vec<T>;
    fn digits(&self) -> Vec<T> {
        self.digits_in_radix(10)
    }

    fn digits_count_in_radix(&self, radix: usize) -> Vec<T>
    where
        usize: TryFrom<T>,
        T: Zero + One + Clone,
        for<'a> &'a T: Add<Output = T>,
    {
        let mut digits_count = vec![T::zero(); radix];
        for digit in self.digits_in_radix(radix) {
            let i = usize::try_from(digit).ok().unwrap();
            digits_count[i] = &digits_count[i] + &T::one();
        }
        digits_count
    }
    fn digits_count(&self) -> Vec<T>
    where
        usize: TryFrom<T>,
        T: Zero + One + Clone,
        for<'a> &'a T: Add<Output = T>,
    {
        self.digits_count_in_radix(10)
    }

    fn is_palindrome_in_radix(&self, radix: usize) -> bool
    where
        T: Clone + Eq,
    {
        let digits = self.digits_in_radix(radix);
        let mut rev_digits = digits.clone();
        rev_digits.reverse();
        digits == rev_digits
    }

    fn is_palindrome(&self) -> bool
    where
        T: Clone + Eq,
    {
        self.is_palindrome_in_radix(10)
    }

    // Digital root of a number is computed by summing digits of a number
    // and repeating that until one-digit number appears.
    fn digital_root_in_radix(&self, radix: usize) -> T
    where
        usize: TryFrom<T>,
        Self: Copy + Sized,
        T: Clone + Eq + Sum + From<Self> + Digits<T>,
    {
        let mut prev = T::from(self.clone());
        let mut this = self.digits_in_radix(radix).into_iter().sum::<T>();
        while this != prev {
            prev = this.clone();
            this = this.digits_in_radix(radix).into_iter().sum();
        }
        this
    }

    fn digital_root(&self) -> T
    where
        usize: TryFrom<T>,
        Self: Copy + Sized,
        T: Clone + Eq + Sum + From<Self> + Digits<T>,
    {
        self.digital_root_in_radix(10)
    }
}

impl<T> Digits<T> for T
where
    T: Integer + Zero + One + Clone + TryFrom<usize> + ToPrimitive + Eq,
    for<'a> &'a T: Sub<Output = T>,
{
    fn digits_in_radix(&self, radix: usize) -> Vec<T> {
        if self == &T::zero() {
            return vec![T::zero()];
        }

        let mut digits = Vec::new();
        let mut _n = if self > &T::zero() {
            self.clone()
        } else {
            &T::zero() - self
        };
        let mut _radix = T::try_from(radix).ok().unwrap();
        while &_n > &T::zero() {
            let (div, rem) = _n.div_rem(&_radix);
            digits.push(rem);
            _n = div;
        }
        digits.into_iter().rev().collect()
    }
}
