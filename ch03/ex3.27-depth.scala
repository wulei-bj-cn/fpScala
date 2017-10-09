def depth(tr: Tree[Int]): Int = tr match {
case Leaf(_) => 1
case Branch(l, r) => 1 + ( depth(l) max depth(r) )
/**
case Branch(l, r) => (l, r) match {
case (Leaf(a), Leaf(b)) => 2
case (Branch(_, _), Leaf(v)) => depth(l)
case (Leaf(v), Branch(_, _)) => depth(r)
case (Branch(_, _), Branch(_, _)) => depth(l) max depth(r)
}
*/
}

