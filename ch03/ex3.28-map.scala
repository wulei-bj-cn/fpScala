def map[A, B](tr: Tree[A])(f: A => B): Tree[B] = tr match {
case Leaf(v) => Leaf(f(v))
case Branch(l, r) => 
map(l)(f)
map(r)(f)
/**
case Branch(l, r) => (l, r) match {
case (Leaf(v1), Leaf(v2)) => Branch(Leaf(f(v1)), Leaf(f(v2)))
case (Leaf(v), Branch(_, _)) => Branch(Leaf(f(v)), map(r)(f))
case (Branch(_, _), Leaf(v)) => Branch(map(l)(f), Leaf(f(v)))
case (Branch(_, _), Branch(_, _)) => Branch(map(l)(f), map(r)(f))
}
*/
}
