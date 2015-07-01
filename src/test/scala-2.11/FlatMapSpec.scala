import org.scalatest.FunSuite

/**
 * Created by amoszhou on 15/7/1.
 */
class FlatMapSpec extends FunSuite {

  test("FlatMap in List") {
    val chars = 'a' to 'z'
    val perms = chars flatMap { a =>
      chars flatMap { b =>
        if (a == b) Seq("%c%c".format(a, b))
        else Seq()
      }
    }
  }

  test("flatmap None"){
    None.flatten

    println(None.map(List(3,_)))
  }
}
