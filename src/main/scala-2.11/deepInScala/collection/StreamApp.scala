package deepInScala.collection

/**
 * Created by amoszhou on 15/7/1.
 */
object StreamApp extends App {

  val s = 1 #:: {
    println("HI")
    2
  } #:: {
    println("BAI")
    3
  } #:: Stream.empty[Int]

  println("---------------------------split-line---------------------------")
  println(s(0))
  println("---------------------------split-line---------------------------")
  println(s(1))
  println("---------------------------split-line---------------------------")
  println(s(2))
//  println(s(3))  IndexOutOfBounds

  val fibs = {
    def f(a:Int,b: Int) : Stream[Int] = a #:: f(b,a+b)
    f(0,1)
  }
}
