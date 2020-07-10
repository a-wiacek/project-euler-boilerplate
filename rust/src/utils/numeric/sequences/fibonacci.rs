pub struct Fibonacci<T> {
    curr: T,
    next: T,
}

impl<T> Fibonacci<T> {
    pub fn new(curr: T, next: T) -> Fibonacci<T> {
        Fibonacci { curr, next }
    }
}

impl<T> Iterator for Fibonacci<T>
where
    T: Clone,
    for<'a> &'a T: std::ops::Add<Output = T>,
{
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        let n = self.curr.clone();
        let new_next = &n + &self.next;
        self.curr = self.next.clone();
        self.next = new_next;

        Some(n)
    }
}
