def map(as: List[Int]): List[Int] = 
foldRight(as, Nil: List[Int])((x, z) => Cons(x + 1, z))
