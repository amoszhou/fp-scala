import org.scalatest.{FunSuite, Matchers}
import datastructure._

/**
 * Created by amoszhou on 15/6/29.
 */
class DataStructureSpec extends FunSuite with Matchers {


  test("reverse") {
    val list = List(1, 2, 3)
    assert(List.reverse(list) == List(3, 2, 1))
  }

  test("Length") {
    val list = List("aa", "bbb", "ccc")
    assert(List.length(list) == 3)
  }

  test("Sm") {
    val result = List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
    println(result)
  }

  test("Sum2 & product2 ") {
    val list = List(1, 2, 3)
    assertResult(6) {
      List.sum2(list)
    }
    val list2 = List(1.0, 2.5, 4.0)
    assertResult(10.0) {
      List.product2(list2)
    }
  }

  test("dropWhile should Drop the element from left util someone not match") {
    val unMatchList = List(8, 1, 2, 3, 4, 5, 6, 7)
    val matchList = List(1, 2, 3, 4, 5, 6, 7, 8)
    assert(List.dropWhile(unMatchList)(_ < 4) == List(8, 1, 2, 3, 4, 5, 6, 7))
    assert(List.dropWhile(matchList)(_ < 4) == List(4, 5, 6, 7, 8))
  }
  test("print any thing") {
    println("msg~!~~")
  }

}
