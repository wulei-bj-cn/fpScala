sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
def zip[A, B](as: List[A], bs: List[B]): List[(A, B)] = (as, bs) match {
case (Nil, _) => Nil: List[(A, B)]
case (_, Nil) => Nil: List[(A, B)]
case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
}
def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
case Nil => z
case Cons(h, t) => f(h, foldRight(t, z)(f))
}
def map[A, B](as: List[A])(f: A => B): List[B] = 
foldRight(as, Nil: List[B])((x, z) => Cons(f(x), z))
def add[A](as: List[A], bs: List[A]): List[A] =
map(zip[A, A](as, bs))({case (a, b) => a + b})
def compute[A, B](as: List[A], bs: List[B])(f: ((A, B)) => B): List[B] = {
 map(zip[A, B](as, bs))(f)
}
