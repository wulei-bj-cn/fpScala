sealed trait Option[+A] {
def map[B](f: A => B): Option[B] = this match {
case None => None
case Some(v) => Some(f(v))
}
def getOrElse[B >: A](default: => B): B = this match {
case None => default
case Some(v) => v
}
def filter(f: A => Boolean): Option[A] = this match {
case None => None
case Some(v) => if (f(v)) Some(v) else None
}
def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
case None => ob
case Some(v) => Some(v)
}
def flatMap[B](f: A => Option[B]): Option[B] = this match {
case None => None
case Some(v) => f(v)
}
def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
case (Some(av), Some(bv)) => Some(f(av, bv))
case _ => None
}
}

case object None extends Option[Nothing]
case class Some[+A](get: A) extends Option[A]
