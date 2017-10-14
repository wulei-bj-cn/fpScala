
import Stream._
import scala.language.postfixOps
sealed trait Stream[+A] {
/**
def unfold[B >: A, S](z: S)(f: S => Option[(B, S)]): Stream[B] = f(z) match {
case Some((b, s)) =>cons(b, unfold(s)(f))
case None => Empty: Stream[B]
}
error: type mismatch;
 found   : S => Option[(B, S)]
 	required: S => Option[(A, S)]
       	case Some((b, s)) =>cons(b, unfold(s)(f))
*/
def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
case Cons(h, t) => f(h(), t().foldRight(z)(f))
case _ => z
}
def append[B >: A](bs: => Stream[B]): Stream[B] = foldRight(bs)((x, z) => cons(x, z))
def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])((x, z) =>
f(x) append z
)
def filter(f: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((x, z) =>
if (f(x)) cons(x, z) else z
)
def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B])((x, z) =>
cons(f(x), z)
)
def forAll(p: A => Boolean): Boolean = foldRight(true)((x, z) => p(x) && z)
def takeWhile(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((x, z) => 
if (p(x)) cons(x, z) else Empty
)
def headOption: Option[A] = foldRight(None: Option[A])((x, _) =>Some(x))
def find(f: A => Boolean): Option[A] = filter(f).headOption
/**
def headOption: Option[A] = foldRight(None: Option[A])((_, z) =>
this match {
case Cons(h, t) => Some(h())
case _ => z
}
)
*/
def exists(p: A => Boolean): Boolean = foldRight(false)((x, z) => p(x) || z)
/**
def exists(p: A => Boolean): Boolean = this match {
case Cons(h, t) => p(h()) || t().exists(p)
case _ => false
}
*/
def take(n: Int): Stream[A] = this match {
case Empty => empty
case Cons(h, t) => 
if (n <= 0) empty
else cons(h(), t().take(n -1))
}
def drop(n: Int): Stream[A] = this match {
case Empty => empty
case Cons(h, t) =>
if (n == 0) cons(h(), t())
else t().drop(n - 1)
}
/**
def takeWhile(p: A => Boolean): Stream[A] = this match {
case Empty => Empty
case Cons(h, t) =>
if (p(h())) cons(h(), t().takeWhile(p))
else Empty
}
*/
def toList: List[A] = this match {
case Empty => Nil
case Cons(h, t) => h() :: t().toList
}
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
object Stream {
def cons[A](h: => A, t: => Stream[A]): Cons[A] = {
lazy val hd = h
lazy val tl = t
Cons(() => hd, () => tl)
}

def empty[A]: Stream[A] = Empty

def apply[A](as: A*): Stream[A] =
if (as.isEmpty) empty
else cons(as.head, apply(as.tail: _*))

def headOption[A](as: Stream[A]): Option[A] = as match {
case Empty => None
case Cons(h, t) => Some(h())
}

/**
def toList[A](as: Stream[A]): List[A] = as match {
case Empty => Nil
case Cons(h, t) => h() :: toList(t())
}
*/

}
