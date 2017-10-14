def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
case Some((a, s)) => Stream.cons(a, unfold(s)(f))
case None => Empty: Stream[A]
}

def map[A, B](as: Stream[A])(f: A => B): Stream[B] = unfold(as)(s => s match {
case Cons(h, t) => Some((f(h()), t()))
case _ => None
})

def take[A](as: Stream[A])(n: Int): Stream[A] = unfold((as, n))(s => s match {
case (Cons(h, t), i) => if (i > 0) Some((h(), (t(), i - 1))) else None
case _ => None
})

def takeWhile[A](as: Stream[A])(f: A => Boolean): Stream[A] = unfold(as)(s => s match {
case Cons(h, t) => if (f(h())) Some((h(), t())) else None
case _ => None
})

def zipWith[A, B](as: Stream[A])(bs: Stream[B]): Stream[(A, B)] = unfold((as, bs))(s => s match {
case (Cons(h1, t1), Cons(h2, t2)) => Some( ( (h1(), h2()), (t1(), t2()) ) )
case _ => None
})

def zipAll[A, B](as: Stream[A])(bs: Stream[B]): Stream[(Option[A],Option[B])] = unfold((as, bs))(s => s match {
case (Cons(h1, t1), Cons(h2, t2)) => Some(( (Some(h1()), Some(h2())), (t1(), t2()) ))
case (Cons(h1, t1), Empty) => Some(( (Some(h1()), None), (t1(), Empty) ))
case (Empty, Cons(h2, t2)) => Some(( (None, Some(h2())), (Empty, t2()) ))
case _ => None
})

