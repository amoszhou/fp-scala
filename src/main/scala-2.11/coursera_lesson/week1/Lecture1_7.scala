package coursera_lesson.week1

import scala.annotation.tailrec

/**
 * Created by amoszhou on 15/7/4.
 */
object Lecture1_7 extends App {


  @tailrec
  def factorial(x: Int, sum: Int = 1): Int = {
    if (x == 0) sum * 1
    else factorial(x - 1, sum * x)
  }

  /**
   * more nice ~~
   * @param x
   * @return
   */
  def factorial_1(x: Int): Int = {
    def loop(acc: Int, n: Int): Int = {
      if (n == 0) acc
      else loop(acc * n, n - 1)
    }
    loop(1, x)
  }

  println(factorial(3))
}
