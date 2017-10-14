def takeWhile[A](as: Stream[A])(p: A => Boolean): Stream[A] = as match {
case Empty => Empty
case Cons(h, t) =>
if (p(h())) cons(h(), takeWhile(t())(p))
else Empty
}
