use num::{integer::ExtendedGcd, Integer};

// Compute modulo inverse a^(-1) mod m

pub trait InvertMod: Sized {
    fn invert_mod(&self, other: &Self) -> Option<Self>;
}

impl<T> InvertMod for T
where
    T: Integer + Clone,
{
    fn invert_mod(&self, other: &Self) -> Option<Self> {
        let ExtendedGcd { gcd, x, .. } = self.extended_gcd(other);
        if gcd == T::one() {
            Some(x.mod_floor(other))
        } else {
            None
        }
    }
}
