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
