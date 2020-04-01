sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
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
    flatMap(a => if (pred(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  private def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap { m =>
      mean(xs map { x =>
        math.pow(x - m, 2)
      })
    }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  def sequence[A](as: List[Option[A]]): Option[List[A]] = as match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
  }

  def sequence1[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight[Option[List[A]]](Some(Nil))((h, t) => map2(h, t)(_ :: _))

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(as map f)

  def traverse1[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse1(t)(f))(_ :: _)
  }

  def traverse1fold[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))
}

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)

    case h :: t => for {
      hh <- h
      tt <- sequence(t)
    } yield hh :: tt
    // h flatMap (hh => sequence(t) flatMap (tt => Right(hh :: tt)))
  }

  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[A]] =
    es.foldRight[Either[E, List[B]]](Right(Nil)) { (e, acc) =>
      f(e).map2(acc)(_ :: _)
    }
}

