sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {

def zip[A, B](as: List[A], bs: List[B]): List[(A, B)] = (as, bs) match {
case (Nil, _) => Nil: List[(A, B)]
case (_, Nil) => Nil: List[(A, B)]
case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
}

def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
case Nil => z
case Cons(h, t) => f(h, foldRight(t, z)(f))
}

/**
def map(as: List[Int])(f: Int => Int): List[Int] = 
foldRight(as, Nil: List[Int])((x, z) => Cons(f(x), z))
*/

def map[A, B](as: List[A])(f: A => B): List[B] = 
foldRight(as, Nil: List[B])((x, z) => Cons(f(x), z))

def compute[A, B](as: List[A], bs: List[B])(f: ((A, B)) => B): List[B] = {
 	map(zip[A, B](as, bs))(f)
}

/**
def add[A](as: List[A], bs: List[A]): A =
map(zip[A, A](as, bs))({case (a, b) => a + b})

def compute[A, B](as: List[A], bs: List[B])(f: (A, B) => B): B = {
val tempList: List[(A, B)] = zip[A, B](as, bs)
 	map(tempList)(f)
}

def filter[A](as: List[A])(f: A => Boolean): List[A] =
foldRight(as, Nil: List[A])(
(x, z) => 
if (f(x)) Cons(x, z)
else z
)
*/

def filter[A](as: List[A])(f: A => Boolean): List[A] =
flatMap(as)(
x => 
if (f(x)) Cons(x, Nil: List[A])
else Nil
)

@annotation.tailrec
def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
case Nil => z
case Cons(h, t) => 
val z_ = f(z, h)
foldLeft(t, z_)(f)
}

def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((xs, y) => Cons(y, xs))

/**
def length[A](as: List[A]): Int = foldRight(as, 0)((_, y) => y + 1)

def sum(l: List[Int]): Int = l match {
case Nil => 0
case Cons(head, tail) => head + sum(tail)
}

def product(l: List[Double]): Double = l match {
case Nil => 1.0
case Cons(0.0, _) => 0.0
case Cons(head, tail) => head * product(tail)
}
*/

def length[A](as: List[A]): Int = foldLeft(as, 0)((x, _) => x + 1)

def sum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

def product(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

def tail[A](l: List[A]): List[A] = l match {
case Nil => Nil
case Cons(head, tail) => tail
}

def append[A](as: List[A], bs: List[A]): List[A] = 
foldRight(as, bs)((x, bs) => Cons(x, bs))

def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
foldRight(as, Nil: List[B])((x, z) => append(f(x), z))

def concat[A](ass: List[List[A]]): List[A] = foldRight(ass, Nil: List[A])(append(_, _))

def setHead[A](l: List[A], h: A): List[A] = l match {
case Nil => Cons(h, Nil)
case Cons(head, tail) => Cons(h, tail)
}

def drop[A](l: List[A], n: Int): List[A] = l match {
case Nil => Nil
case Cons(head, tail) => 
if (n == 1) tail
else drop(tail, n - 1)
}

def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
case Nil => Nil
case Cons(head, tail) => 
if (f(head)) tail
else dropWhile(tail, f)
}

def init[A](l: List[A]): List[A] = l match {
case Nil => Nil
case Cons(head, tail) =>
tail match {
case Nil => Nil
case Cons(_, _) => Cons(head, init(tail))
}
}

def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
case (Nil, _) => false
case (_, Nil) => false
case (Cons(h1, t1), Cons(h2, Nil)) => 
if (h1 == h2) true
else false
case (Cons(h1, t1), Cons(h2, t2)) => 
if (h1 == h2) hasSubsequence(t1, t2)
else hasSubsequence(t1, sub)
}

def contains[A](as: List[A])(x: A): Boolean = as match {
case Nil => false
case Cons(h, t) => if (h == x) true else contains(t)(x)
}

def apply[A](as: A*): List[A] = {
if (as.isEmpty) Nil
else Cons(as.head, apply(as.tail: _*))
}
}

val x = List(1,2,3,4,5) match {
case Cons(x, Cons(2, Cons(4, _))) => x
case Nil => 42
case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y 
case Cons(h, t) => h + sum(t)
case _ => 101
}

