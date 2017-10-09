sealed trait Tree[+A]
case class Leaf[A](leaf: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def size[A](tr: Tree[A]): Int = tr match {
case Leaf(_) => 1
case Branch(left, right) => size(left) + size(right) 
}
