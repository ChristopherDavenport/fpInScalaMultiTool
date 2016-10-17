package fpinscalamulti.Vanilla.parallelism

import java.util.concurrent.{Callable, ExecutorService, Future}

import scala.concurrent.duration.TimeUnit

/**
  * Created by davenpcm on 10/16/16.
  */
//trait Par[A] {
//
//
//}

object Par {
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private[Par] case class UnitFuture[A](get: A) extends Future[A]{
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A,B,C](p1: Par[A])(p2: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val p1f = p1(es)
      val p2f = p2(es)
      UnitFuture(f(p1f.get, p2f.get))
    }
  def fork[A](a: => Par[A]): Par[A] =
    (es: ExecutorService) => es.submit(new Callable[A]{
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)


}
