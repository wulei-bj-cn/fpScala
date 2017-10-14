sealed trait Either[+E, +A] {
def map[B](f: A => B): Either[E, B] = this match {
case Left(e) => Left(e): Either[E, B]
case Right(a) => Right(f(a))
}
def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
case Left(e) => Left(e): Either[EE, B]
case Right(a) => f(a)
}
def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
case Left(e) => Left(e): Either[EE, B]
case Right(a) => b
}
def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
this flatMap(aa => b map(bb => f(aa, bb)))

}
case class Left[+E](e: E) extends Either[E, Nothing]
case class Right[+A](a: A) extends Either[Nothing, A]
