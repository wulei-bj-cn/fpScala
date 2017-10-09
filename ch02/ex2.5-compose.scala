def compose[A,B,C](f: B => C, g: A => B): A => C =
a => f(g(a))

def compose[A,B,C](f: B => C, g: A => B): A => C =
f compose g

def compose[A,B,C](f: B => C, g: A => B): A => C =
g andThen f
