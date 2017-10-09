def variance(xs: Seq[Double]): Option[Double] = 
if (xs.isEmpty) None
else Some(xs).flatMap(x => Some(xs, xs.sum / xs.size)).flatMap({ case (seq, m) => Some(seq.map(x => math.pow(x - m, 2)).sum / seq.size)})

def variance(xs: Seq[Double]): Option[Double] = 
if (xs.isEmpty) None
else {
Some(xs).map(x => (xs, xs.sum / xs.size)).map({ case (seq, m) => seq.map(x => math.pow(x - m, 2)).sum / seq.size})
}

def variance(xs: Seq[Double]): Option[Double] =
if (xs.isEmpty) None
else {
val m = xs.sum / xs.size
val v = xs.map(x => math.pow(x - m, 2))
Some(v.sum / v.size)
}

