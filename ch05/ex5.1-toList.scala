def toList[A](as: Stream[A]): List[A] = as match {
case Empty => Nil
case Cons(h, t) => h() :: toList(t())
}
