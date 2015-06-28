package funcational

/**
 * Created by amoszhou on 15/6/25.
 */
object MyModule {


  def abs(x: Int): Int = if (x < 0) -x else x


  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }


  private def formatAbs(x: Int) = {
    val msg = "the absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  //  private def formatFactorial(n: Int) = {
  //    val msg = "The factorial of %d is %d."
  //    msg.format(n, factorial(n))
  //  }

  def main(args: Array[String]): Unit = {
    println(formatResult("abs",-42,abs))
    //    println(formatFactorial(7))



    val x = List(1,2,3,4,5) match {
      case x :: 2 :: 4 :: Nil => x
      case Nil => 42
      case x :: y :: 3 :: 4 :: _ => x+y
      case h :: t => h + sum(t)
      case _ => 101
    }

    print(x)
  }

  def sum(list: List[Int]) : Int= list match {
    case Nil => 0
    case x :: xs => x + sum(xs)
  }

}

