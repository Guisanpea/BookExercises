import scala.collection.immutable.{AbstractMap, SeqMap, SortedMap}

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

val stringMonoid = new Monoid[String] {
  def op(a1: String, a2: String): String = a1 + a2

  val zero = ""
}

def listMonoid[A] = new Monoid[List[A]] {
  override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

  val zero = Nil
}

val intAddition: Monoid[Int] = new Monoid[Int] {
  def op(i1: Int, i2: Int): Int = i1 + i2

  val zero = 0
}

val intMultiplication: Monoid[Int] = new Monoid[Int] {
  override def op(a1: Int, a2: Int) = a1 * a2

  override def zero = 1
}

val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
  override def op(a1: Boolean, a2: Boolean) = a1 && a2

  override def zero = true
}

val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
  override def op(a1: Boolean, a2: Boolean) = a1 || a2

  override def zero = false
}

def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
  override def op(a1: Option[A], a2: Option[A]) = a1 orElse a2

  override def zero = None
}

def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
  override def op(a1: A => A, a2: A => A) = a1 andThen a2

  override def zero = identity
}

def concatenate[A, B](as: List[A], m: Monoid[A]): A =
  as.foldLeft(m.zero)(m.op)

def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
  as.foldLeft(m.zero)((acc, a) => m.op(acc, f(a)))

def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
  foldMap(as, endoMonoid[B])(f.curried)(z)

def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = as.length match {
  case 0 => m.zero
  case 1 => f(as(0))
  case _ =>
    val (l, r) = as.splitAt(as.length / 2)
    m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
}

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

val wcMonoid: Monoid[WC] = new Monoid[WC] {
  private def wordsOnConcat(l: String, r: String) =
    if ((l + r).isEmpty) 0 else 1

  override def op(wc1: WC, wc2: WC): WC = (wc1, wc2) match {
    case (Stub(l), Stub(r)) => Stub(l + r)
    case (Stub(ls), Part(l, n, r)) => Part(ls + l, n, r)
    case (Part(l, n, r), Stub(rs)) => Part(l, n, r + rs)
    case (Part(l1, n1, r1), Part(l2, n2, r2)) =>
      Part(
        l1,
        n1 + wordsOnConcat(r1, l2) + n2,
        r2
      )
  }

  override def zero = Stub("")
}

def countWords(s: String): Int = {
  def charToWc(c: Char): WC =
    if (c.isWhitespace) Part("", 0, "")
    else Stub(c.toString)

  foldMapV(s.toIndexedSeq, wcMonoid)(charToWc) match {
    case Stub(_) => 1
    case Part("", n, "") => n
    case Part("", n, _) => n + 1
    case Part(_, n, _) => n + 2
  }
}

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]): B

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(m: Monoid[B]) =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: Stream[A])(f: A => B)(m: Monoid[B]): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = as match {
    case Leaf(a) => f(a, z)
    case Branch(l, r) => foldRight(l)(
      foldRight(r)(z)(f)
    )(f)
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match {
    case Leaf(a) => f(z, a)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }

  override def foldMap[A, B](as: Tree[A])(f: A => B)(m: Monoid[B]) = as match {
    case Leaf(a) => f(a)
    case Branch(l, r) => m.op(foldMap(l)(f)(m), foldMap(r)(f)(m))
  }
}

object optionFoldable extends Foldable[Option] {
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match {
    case Some(value) => f(value, z)
    case None => z
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as match {
    case Some(value) => f(z, value)
    case None => z
  }

  override def foldMap[A, B](as: Option[A])(f: A => B)(m: Monoid[B]) = as match {
    case Some(value) => f(value)
    case None => m.zero
  }
}

def functionMonoid[A, B](bm: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
  override def op(f1: A => B, f2: A => B): A => B = a => bm.op(f1(a), f2(a))

  override def zero = _ => bm.zero
}
