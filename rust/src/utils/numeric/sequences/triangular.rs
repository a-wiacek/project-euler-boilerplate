use num::One;

pub struct Triangular<T> {
    counter: T,
    result: T,
}

impl<T: One> Triangular<T> {
    pub fn new() -> Triangular<T> {
        Triangular {
            counter: T::one(),
            result: T::one(),
        }
    }
}

impl<T> Iterator for Triangular<T>
where
    T: Clone + One,
    for<'a> &'a T: std::ops::Add<Output = T>,
{
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        let n = self.result.clone();
        self.counter = &self.counter + &T::one();
        self.result = &self.result + &self.counter;
        Some(n)
    }
}
