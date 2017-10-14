def from(n: Int) = unfold(n)(s => Some((s, s + 1)))
def constant(n: Int) = unfold(n)(s => Some((s, s)))
def ones: Stream[Int] = unfold(1)(s => Some((s, s)))

