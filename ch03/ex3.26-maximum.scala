def maximum(tr: Tree[Int]): Int = tr match {
case Leaf(v) => v
case Branch(l, r) => maximum(l) max maximum(r)
/**
case Branch(l, r) => (l, r) match {
case (Leaf(a), Leaf(b)) => a max b
case (Branch(_, _), Leaf(v)) => maximum(l) max v
case (Leaf(v), Branch(_, _)) => maximum(r) max v
case (Branch(_, _), Branch(_, _)) => maximum(l) max maximum(r)
}
*/
}
