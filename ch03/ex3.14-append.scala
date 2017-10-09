def append[A](as: List[A], bs: List[A]): List[A] = 
foldRight(as, bs)((x, bs) => Cons(x, bs))
