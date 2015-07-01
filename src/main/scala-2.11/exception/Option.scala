package exception

import scala.util.Try

/**
 * Created by amoszhou on 15/6/30.
 */
sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(value) => Some(f(value))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(value) => value
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

}

case class Some[+A](value: A) extends Option[A]

case object None extends Option[Nothing]


object Option {

  /**
   * 平均数，不考虑精度丢失的问题
   * @param seq
   * @return
   */
  def mean(seq: Seq[Double]): Option[Double] = {
    if (seq.isEmpty) None
    else Some(seq.sum / seq.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))


  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map (f)

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(v1 => b.map((v2: B) => f(v1, v2)))

    //    for {
    //      v1 <- a
    //      v2 <- b
    //    } yield f(v1, v2)
  }


}

object TestApp extends App {
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil) //Can't use None
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
  }

  val l = List(Some(1), Some(2), Some(3), Some(5))
  println(sequence(l))
}