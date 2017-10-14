def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))
