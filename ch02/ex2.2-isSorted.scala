def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
@annotation.tailrec
def go(n: Int, sorted: Boolean): Boolean = 
if (!sorted) sorted
else if (n > as.length) sorted
else go(n + 1, ordered(as(n), as(n + 1)) && sorted)
go(0, true)
}
