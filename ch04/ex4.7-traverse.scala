def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
val foldedList: List[B] = foldRight(as, Nil: List[B])((x, z) => f(x) match {
case Left(e) => return Left(e): Either[E, List[B]]
case Right(a) => Cons(a, z)
})
Right(foldedList)
}
def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(x => x)
