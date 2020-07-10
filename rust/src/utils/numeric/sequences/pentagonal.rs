use num::One;

pub struct Pentagonal<T> {
    counter: T,
    result: T,
}

impl<T: One> Pentagonal<T> {
    pub fn new() -> Pentagonal<T> {
        Pentagonal {
            counter: T::one(),
            result: T::one(),
        }
    }
}

impl<T> Iterator for Pentagonal<T>
where
    T: Clone + One,
    for<'a> &'a T: std::ops::Add<Output = T>,
{
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        let n = self.result.clone();
        self.result =
            &(&(&(&self.result + &self.counter) + &self.counter) + &self.counter) + &T::one();
        self.counter = &self.counter + &T::one();
        Some(n)
    }
}
