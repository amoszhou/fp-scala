package coursera_lesson.week2

/**
 * Created by amoszhou on 15/7/5.
 */
object Lecture2_2 extends App {

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }

  def fact(n: Int) = product(x => x)(1, n)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
  }

  val product3And4 = product(x => x * x)(3, 4)
  println(product3And4)

  println(fact(5))
}
