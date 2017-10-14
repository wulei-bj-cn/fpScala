def fib(n: Int): Int = {
@annotation.tailrec
def go(n: Int, a: Int, b: Int): Int = {
if(n <= 0) b
else go(n - 1, b, b + a)
}
go(n, 1, 0)
}
