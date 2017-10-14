def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
case Some((a, s)) => Stream.cons(a, unfold(s)(f))
case None => Empty: Stream[A]
}
def fibs: Stream[Int] = unfold((0, 1))(s => {
val a = s._1 + s._2
Some((a, (s._2, a)))
})
