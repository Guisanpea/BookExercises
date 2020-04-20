trait Functor[F[_]] {
  def map[A, B](fa : F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match {
      case Left(fa) => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)( b => f(a, b) ))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((a, acc) => map2(a, acc)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((a, acc) => map2(f(a), acc)(_ :: _))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    if (n < 1) unit(Nil)
    else map2(ma, replicateM(n-1, ma))(_ :: _)
  }

  def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ms match {
    case h :: t => flatMap(f(h))(
      if (_) map2(unit(h), filterM(t)(f))(_ :: _)
      else filterM(t)(f)
    )
    case Nil => unit(Nil)
  }

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def _flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] ={
    val g = (_: Unit) => ma
    compose(g, f)(())
  }

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(identity)
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)

}
