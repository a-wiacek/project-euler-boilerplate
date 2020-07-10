pub use itertools::Itertools as BaseItertools;
// Return an iterator that iterates over pairs: `k`-length combinations
// of the elements from an iterator and complements
// (corresponding `n - k`-length combinations).
// This iterator is strict (it collects all items upon initializing).

#[derive(Clone)]
#[must_use = "iterator adaptors are lazy and do nothing unless consumed"]
pub struct DoubleCombinations<I: Iterator> {
    elements: Vec<I::Item>,
    indices: Vec<usize>,
    first: bool,
}

fn double_combinations<I>(iter: I, k: usize) -> DoubleCombinations<I>
where
    I: Iterator,
{
    DoubleCombinations {
        elements: iter.collect(),
        indices: (0..k).collect(),
        first: true,
    }
}

impl<I> Iterator for DoubleCombinations<I>
where
    I: Iterator,
    I::Item: Clone,
{
    type Item = (Vec<I::Item>, Vec<I::Item>);
    fn next(&mut self) -> Option<Self::Item> {
        if self.first {
            if self.indices.len() > self.elements.len() {
                return None;
            }
            self.first = false;
        } else if self.indices.len() == 0 {
            return None;
        } else {
            let mut i: usize = self.indices.len() - 1;

            while self.indices[i] == i + self.elements.len() - self.indices.len() {
                if i > 0 {
                    i -= 1;
                } else {
                    // Reached the last combination
                    return None;
                }
            }

            // Increment index, and reset the ones to its right
            self.indices[i] += 1;
            for j in i + 1..self.indices.len() {
                self.indices[j] = self.indices[j - 1] + 1;
            }
        }

        let mut left = Vec::new();
        let mut right = Vec::new();
        let mut index = 0;
        for i in 0..self.elements.len() {
            if index < self.indices.len() && self.indices[index] == i {
                left.push(self.elements[i].clone());
                index += 1;
            } else {
                right.push(self.elements[i].clone());
            }
        }
        Some((left, right))
    }
}

pub trait Itertools: BaseItertools {
    fn double_combinations(self, k: usize) -> DoubleCombinations<Self>
    where
        Self: Sized,
        Self::Item: Clone,
    {
        double_combinations(self, k)
    }
}

impl<T: ?Sized> Itertools for T where T: BaseItertools {}
