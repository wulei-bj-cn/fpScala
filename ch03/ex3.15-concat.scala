def concat[A](ass: List[List[A]]): List[A] = foldLeft(ass, Nil)(append(_, _))
