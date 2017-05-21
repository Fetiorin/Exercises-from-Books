package c11_monads

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]
  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))
  def sequence[A](lma: List[M[A]]): M[List[A]] = traverse(lma)(x => x)
  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(Nil): M[List[B]])((cur, acc) => map2(f(cur), acc)(_ :: _))
  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = traverse((1 to n).toList)(_ => ma)
  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = (a: A) => flatMap(f(a))(g)
  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = compose((_:Unit) => ma, f)(())
}

object Monad {
  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A, B](o: Option[A])(f: A => Option[B]): Option[B] = o flatMap f
  }


}
