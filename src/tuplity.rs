//#[derive(Debug,Clone)]
//enum TwoOrMore<T> {
//    Two(T, T),
//    More(T, Box<TwoOrMore<T>>)
//}

#[derive(Debug,Clone)]
pub enum Tuplity<T> {
    Single(T),
//    Multi(TwoOrMore<Box<Tuplity<T>>>)
}

//impl<T> TwoOrMore<T> {
//    fn map<R, F>(&self, f: &F) -> TwoOrMore<R> where F: Fn(&T)->R {
//        match self {
//            &TwoOrMore::Two(ref a, ref b) => {
//                TwoOrMore::Two(f(a), f(b))
//            }
//            &TwoOrMore::More(ref a, ref tail) => {
//                TwoOrMore::More(f(a), box tail.map(f))
//            }
//        }
//    }
//}
//
//fn crap<T, R, F>(tup: &Box<Tuplity<T>>, f: &F) -> Box<Tuplity<R>> where F: Fn(&T)->R {
//    box tup.map(f)
//}
//
//impl<T> Tuplity<T> {
//    pub fn map<R, F>(&self, f: &F) -> Tuplity<R> where F: Fn(&T)->R {
//        match self {
//            &Tuplity::Single(ref v) => Tuplity::Single(f(v)),
//            &Tuplity::Multi(ref tom) => { // tom: &TwoOrMore<Box<Tuplity<T>>>
//                Tuplity::Multi(tom.map(&|tup| crap(tup, f)))
//            }
//        }
//    }
//}

