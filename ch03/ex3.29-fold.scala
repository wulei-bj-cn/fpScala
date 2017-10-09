sealed trait Tree[+A]
case class Leaf[A](leaf: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

//利用高阶函数从具体到抽象；用高阶函数提取公共、通用部分
def fold[A, B](tr: Tree[A])(g: A => B)(f: (B, B) => B): B = tr match {
case Leaf(v) => g(v)
case Branch(l, r) => f(fold(l)(g)(f), fold(r)(g)(f))
}

def size[A](tr: Tree[A]): Int = fold(tr)(_ => 1)(_ + _)
def maximum(tr: Tree[Int]): Int = fold(tr)(x => x)(_ max _)
def depth[A](tr: Tree[A]): Int = fold(tr)(_ => 1)((l, r) => (l max r) + 1)
def map[A, B](tr: Tree[A])(f: A => B): Tree[B] = fold(tr)(x => Leaf(f(x)).asInstanceOf[Tree[B]])(Branch[B](_, _))
