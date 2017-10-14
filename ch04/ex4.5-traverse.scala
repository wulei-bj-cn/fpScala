def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
val foldedList = foldRight(a, Nil: List[B])((x, z) => f(x) match {
case None => return None
case Some(v) => Cons(v, z)
}
)
Some(foldedList)
}

def sequence[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)
