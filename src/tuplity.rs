enum TwoOrMore<T> {
    Two(T, T),
    More(T, Box<TwoOrMore<T>>)
}

enum Tuplity<T> {
    Single(T),
    Multi(TwoOrMore<Box<Tuplity<T>>>)
}

impl<T> TwoOrMore<T> {
    fn map<R, F>(&self, f: F) -> TwoOrMore<R> where F: Fn(&T)->R {
        match self {
            &TwoOrMore::Two(ref a, ref b) => {
                TwoOrMore::Two(f(a), f(b))
            }
            &TwoOrMore::More(ref a, ref tail) => {
                TwoOrMore::More(f(a), box tail.map(f))
            }
        }
    }
}

impl<T> Tuplity<T> {
    fn map<R, F>(&self, f: F) -> Tuplity<R> where F: Fn(&T)->R {
        match self {
            &Tuplity::Single(ref v) => Tuplity::Single(f(v)),
            &Tuplity::Multi(ref vals) => {
                Tuplity::Multi(vals.map(f))
            }
        }
    }
}
