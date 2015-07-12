package fpdesign

import java.util.concurrent.ExecutorService

/**
 * Created by amoszhou on 15/7/2.
 */
class ChooseDataType {

}


object ChooseDataType {

  def sum(ints: IndexedSeq[Int]): Int = {
    if (ints.size <= 1)
      ints.headOption.getOrElse(0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum(l) + sum(r)
    }
  }
}

class Par[+A]

object Par {
  def unit[A](a: => A): Par[A] = {
    new Par[A]
  }

//  def map2[A](a: Par[A], b: Par[A])(f: (A, A) => A)

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
//    (es: ExecutorService) => {
//      val af = a(es)
//      val bf = b(es)
//      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
//    }


  def get[A](par: Par[A]): A =


  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption.getOrElse(0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val sumL: Par[Int] = Par.unit(sum(l))
      val sumR: Par[Int] = Par.unit(sum(r))
      Par.get(sumL) + Par.get(sumR)
    }

  //  def sum(ints: IndexedSeq[Int]): Par[Int]={
  //    if (ints.size <= 1)
  //      Par.unit(ints.headOption getOrElse 0) else {
  //      val (l,r) = ints.splitAt(ints.length/2)
  //      Par.map2(sum(l), sum(r))(_ + _)
  //  }
}
