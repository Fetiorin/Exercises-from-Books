package c04_handlingerrors

import java.util.regex._

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = flatMap(op => if (f(op)) Some(op) else None)


}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Exercises extends App {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a flatMap(x => b map (y => f(x, y)))

  /**
    * pattern and mkMatcher functions are cmd+c & cmd+v from book
    */

  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher(pat: String): Option[String => Boolean] =
    pattern(pat) map (p => (s: String) => p.matcher(s).matches)


//  def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] =
//    for {
//      f <- mkMatcher(pat)
//      g <- mkMatcher(pat2)
//    } yield f(s) && g(s)

  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = map2(mkMatcher(pat1), mkMatcher(pat2))((a, b) => a(s) && b(s))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case x :: xs => x flatMap (y => sequence(xs) map (y :: _))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]]  =
    a.foldRight[Option[List[B]]](Some(Nil)){
      (cur, acc) =>
        f(cur) flatMap (x => acc map (x :: _))
    }

  def seqViaTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)

}
