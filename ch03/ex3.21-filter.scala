def filter[A](as: List[A])(f: A => Boolean): List[A] =
flatMap(as)(
x => 
if (f(x)) Cons(x, Nil: List[A])
else Nil
)
