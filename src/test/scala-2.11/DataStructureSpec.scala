import org.scalatest.{FunSuite, Matchers, FlatSpec}
import datastructure._

/**
 * Created by amoszhou on 15/6/29.
 */
class DataStructureSpec extends FunSuite with Matchers {


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
