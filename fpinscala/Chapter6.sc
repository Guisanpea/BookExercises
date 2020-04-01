trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, r) = rng.nextInt
    if (n >= 0) (n, r)
    else (-(n + 1), r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, r) = nonNegativeInt(rng)
    (n / Int.MaxValue, r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, r) = rng.nextInt
    val (d, r2) = double(r)
    ((n, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @scala.annotation.tailrec
    def ints(count: Int, irng: RNG, acc: List[Int]): (List[Int], RNG) =
      if (count <= 0) (acc, irng)
      else {
        val (i, r) = irng.nextInt
        ints(count - 1, r, i :: acc)
      }

    ints(count, rng, Nil)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = (a, _)

  def map[A, B](ra: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, r) = ra(rng)
    (f(a), r)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def double2: Rand[Double] = map(nonNegativeInt)(_ / Int.MaxValue)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, r) = ra(rng)
    val (b, r2) = rb(r)
    (f(a, b), r2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((ra, acc) => map2(ra, acc)(_ :: _))

  def intsSequenced(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(int))(rng)

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = rng => {
    val (a, r1) = r(rng)
    f(a)(r1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - i >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }
  }

  def mapFlatmapped[A, B](ra: Rand[A])(f: A => B): Rand[B] = flatMap(ra)(a => unit(f(a)))

  def map2Flatmapped[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a =>
    mapFlatmapped(rb)(b =>
      f(a, b)
    )
  )
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })

}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State((a, _))

  def sequence[S, A](lsa: List[State[S, A]]): State[S, List[A]] =
    lsa.foldRight(unit[S, List[A]](List()))(
      (sa, acc) => sa.map2(acc)(_ :: _)
    )
}
