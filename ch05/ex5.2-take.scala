def take[A](as: Stream[A])(n: Int): Stream[A] = as match {
case Empty => empty
case Cons(h, t) => 
if (n <= 0) empty
else cons(h(), take(t())(n -1))
}

def drop[A](as: Stream[A])(n: Int): Stream[A] = as match {
case Empty => empty
case Cons(h, t) =>
if (n == 0) cons(h(), t())
else drop(t())(n - 1)
}
