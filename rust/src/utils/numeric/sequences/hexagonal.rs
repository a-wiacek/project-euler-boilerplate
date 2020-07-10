use num::One;

pub struct Hexagonal<T> {
    counter: T,
    result: T,
}

impl<T: One> Hexagonal<T> {
    pub fn new() -> Hexagonal<T> {
        Hexagonal {
            counter: T::one(),
            result: T::one(),
        }
    }
}

impl<T> Iterator for Hexagonal<T>
where
    T: Clone + One,
    for<'a> &'a T: std::ops::Add<Output = T>,
{
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        let n = self.result.clone();
        self.result = &(&(&(&(&self.result + &self.counter) + &self.counter) + &self.counter)
            + &self.counter)
            + &T::one();
        self.counter = &self.counter + &T::one();
        Some(n)
    }
}
