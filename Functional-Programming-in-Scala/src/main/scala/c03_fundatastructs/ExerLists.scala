package c03_fundatastructs

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  /**
    * Part down here copied from book
    */

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  /**
    * Exercise 02
    */

  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, xs) => xs
    case Nil => throw new Exception("Tail of empty list")
  }

  /**
    * Exercise 03
    */

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Cons(_, xs) => drop(xs, n-1)
      case Nil => Nil
    }
  }

  /**
    * Exercise 04
    * With implicit param.
    */

  def dropWhile[A](l: List[A])(implicit p: A => Boolean): List[A] = l match {
    case Cons(x, xs) if p(x) => dropWhile(xs)
    case _ => l
  }

  /**
    * Exercise 05
    */

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => throw new Exception("Head of empty list")
    case Cons(_, hs) => Cons(h, hs)
  }

  /**
    * Exercise 05
    */

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new Exception("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  /**
    * Exercise 9
    */

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, b: Int) => b + 1)

  /**
    * Exercise 10
    */

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  /**
    * Exercise 11
    */

  def sumLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def productLeft(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  def lengthLeft[A](l: List[A]): Int = foldLeft(l, 0)((b: Int, _) => b + 1)

  /**
    * Exercise 12
    */

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A])((xs, x) => Cons(x, xs))

  /**
    * Exercise 13
    * FIX: add foldLeft in terms of foldRight
    */

  def foldRightWithLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = foldLeft(reverse(l), z)(f)

  /**
    * Exercise 14
    */

  def appendWithFold[A](l1: List[A], l2: List[A]) = foldRight(l1, l2)((x, xs) => Cons(x, xs))

  /**
    * Exercise 15
    */

  def flatten[A](l: List[List[A]]): List[A] = foldRight(l, Nil:List[A])((x, xs) => appendWithFold(x, xs))

  /**
    * Exercise 16
    */

  def add1(l: List[Int]): List[Int] = foldRight(l, Nil:List[Int])((x, xs) =>  Cons(x+1, xs))

  /**
    * Exercise 17
    */

  def doubleToStr(l: List[Double]): List[String] = foldRight(l, Nil:List[String])((x, xs) =>  Cons(x.toString, xs))

  /**
    * Exercise 18
    */

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil:List[B])((x, xs) => Cons(f(x), xs))

  /**
    * Exercise 19
    */

  def filter[A](l: List[A])(p: A => Boolean): List[A] = l match {
    case Cons(x, xs) if p(x) => Cons(x, filter(xs)(p))
    case Cons(_, xs) => filter(xs)(p)
    case Nil => Nil
  }

  /**
    * Exercise 20
    */

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    flatten(map(l)(f))

  /**
    * Exercise 21
    */

  def filterWithFlatMap[A](l: List[A])(p: A => Boolean): List[A] = flatMap(l)(x => if (p(x)) List(x) else Nil)

  /**
    * Exercise 22
    */

  def addList(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x+y, addList(xs, ys))
  }

  def combine[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), combine(xs, ys)(f))
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    def isSubsequnce(l: List[A], subs: List[A]): Boolean = (l, subs) match {
      case (_, Nil) => true
      case (Cons(l, ls), Cons(x, xs)) if (l == x) => isSubsequnce(ls, xs)
      case _ => false
    }
    def loop(l: List[A]): Boolean = l match {
      case Nil => false
      case list@Cons(x, xs) =>
        if (isSubsequnce(list, sub)) true
        else loop(xs)
    }
    loop(l)
  }

}
