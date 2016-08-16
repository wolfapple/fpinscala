import scala.{Option => _, Either => _, Left => _, Right => _, _}

object Ch4 extends App {

//  sealed trait List[+A]
//
//  case object Nil extends List[Nothing]
//
//  case class Cons[+A](head: A, tail: List[A]) extends List[A]
//
//  object List {
//    def sum(ints: List[Int]): Int = ints match {
//      case Nil => 0
//      case Cons(x, xs) => x + sum(xs)
//    }
//
//    def product(ds: List[Double]): Double = ds match {
//      case Nil => 1.0
//      case Cons(0.0, _) => 0.0
//      case Cons(x, xs) => x * product(xs)
//    }
//
//    def tail[A](l: List[A]): List[A] = l match {
//      case Nil => sys.error("tail of empty list")
//      case Cons(_, t) => t
//    }
//
//    def setHead[A](l: List[A], h: A): List[A] = l match {
//      case Nil => sys.error("setHead on empty list")
//      case Cons(_, t) => Cons(h, t)
//    }
//
//    @annotation.tailrec
//    def drop[A](l: List[A], n: Int): List[A] =
//      if (n <= 0) l
//      else drop(tail(l), n - 1)
//
//    @annotation.tailrec
//    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
//      case Cons(h, t) if f(h) => dropWhile(t, f)
//      case _ => l
//    }
//
//    def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
//      case Nil => a2
//      case Cons(h, t) => Cons(h, append(t, a2))
//    }
//
//    def init[A](l: List[A]): List[A] = l match {
//      case Nil => sys.error("init of empty list")
//      case Cons(_, Nil) => Nil
//      case Cons(h, t) => Cons(h, init(t))
//    }
//
//    def apply[A](as: A*): List[A] =
//      if (as.isEmpty) Nil
//      else Cons(as.head, apply(as.tail: _*))
//  }

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
      this match {
        case Left(e) => Left(e)
        case Right(a) => f(a)
      }

    def orElse[EE >: E, B >: A](b: Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for {
        a <- this
        b1 <- b
      } yield f(a, b1)
  }

  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]

  object Either {
    def traverse[E, A, B](es: List[A])(
        f: A => Either[E, B]): Either[E, List[B]] =
      es match {
        case Nil => Right(Nil)
        case h :: t => (f(h) map2 traverse(t)(f))(_ :: _)
      }
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
      traverse(es)(x => x)
  }
}
