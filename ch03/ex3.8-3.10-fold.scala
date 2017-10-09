def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
case Nil => z
case Cons(h, t) => f(h, foldRight(t, z)(f))
}
def length[A](as: List[A]): Int = foldRight(as, 0)((_, y) => y + 1)

@annotation.tailrec
def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
case Nil => z
case Cons(h, t) => 
val z_ = f(z, h)
foldLeft(t, z_)(f)
}
