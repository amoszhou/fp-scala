package exception

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

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

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
}