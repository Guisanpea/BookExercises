import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](ec: ExecutorService)(p: Par[A]): Future[A] = p(ec)

  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = { es: ExecutorService =>
    val fa = pa(es)
    val fb = pb(es)
    UnitFuture(f(fa.get(), fb.get()))
  }

  case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps match {
    case h :: t => map2(h, fork( sequence(t) ))(_ :: _)
    case Nil => unit(Nil)
  }
}