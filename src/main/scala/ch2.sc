def fib(n: Int): Int = {
  @annotation.tailrec
  def loop(n: Int, prev: Int, cur: Int): Int =
    if (n == 0) prev
    else loop(n -1, cur, prev + cur)

  loop(n, 0, 1)
}
fib(10)

def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
  @annotation.tailrec
  def go(n: Int): Boolean =
    if (n >= as.length - 1) true
    else if (gt(as(n), as(n + 1))) false
    else go(n + 1)

  go(0)
}
isSorted(Array(1, 2, 3), (a: Int, b: Int) => a < b)

def curry[A,B,C](f: (A, B) => C): A => (B => C) =
  a => b => f(a, b)

def uncurry[A,B,C](f: A => B => C): (A, B) => C =
  (a, b) => f(a)(b)

def compose[A,B,C](f: B => C, g: A => B): A => C =
  a => f(g(a))