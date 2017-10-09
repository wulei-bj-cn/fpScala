def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
foldRight(as, Nil: List[B])((x, z) => append(f(x), z))
