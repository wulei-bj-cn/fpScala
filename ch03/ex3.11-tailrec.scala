@annotation.tailrec
def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
case Nil => z
case Cons(h, t) => 
val z_ = f(z, h)
foldLeft(t, z_)(f)
}

def length[A](as: List[A]): Int = foldLeft(as, 0)((x, _) => x + 1)

def sum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

def product(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)
