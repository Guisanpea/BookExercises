sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some( f(a) )
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def flatMapWithoutPatternMatching[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  def filter(pred: A => Boolean): Option[A] =
    flatMap( a => if (pred(a)) Some(a) else None )
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  private def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum /xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap { m =>
        mean(xs map { x =>
            math.pow(x - m, 2)
        })
    }
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  def sequence[A](as: List[Option[A]]): Option[List[A]] = as match {
    case Nil => Some(Nil)
    case h :: t => h flatMap ( hh => sequence(t) map (hh :: _) )
  }

  def sequence1[A](as: List[Option[A]]): Option[List[A]] =
    as.
    foldRight[Option[List[A]]]( Some(Nil) )( (h, t) => map2(h, t)(_ :: _) )

  def traverse[A,B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(as map f)

  def traverse1[A,B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse1(t)(f))(_ :: _)
  }

  def traverse1[A,B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight[Option[List[A]]]( Some(Nil) )( (h,t) => map2(f(h), t)(_ :: _) )
}

sealed trait Either[+E, +A]
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

