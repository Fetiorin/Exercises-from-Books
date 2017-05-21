package c10_monoids

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoids {

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    def zero = false
  }
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    def zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
    override def zero = None
  }

  def EndoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    override def op(a1: (A) => A, a2: (A) => A) = a1 compose a2
    override def zero = (a: A) => a
  }

  def wordsMonoid(s: String): Monoid[String] = new Monoid[String] {
    override def op(a1: String, a2: String) = a1.trim + " " + a2.trim
    override def zero = ""
  }

}
