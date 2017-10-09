def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((xs, y) => Cons(y, xs))
