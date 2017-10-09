def filter[A](as: List[A])(f: A => Boolean): List[A] =
foldRight(as, Nil: List[A])(
(x, z) => 
if (f(x)) Cons(x, z)
else z
)
