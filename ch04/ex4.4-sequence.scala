def sequence[A](a: List[Option[A]]): Option[List[A]] = 
if (contains(a)(None)) None
else Some(map(a)(x => x.get))

