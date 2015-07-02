package deepInScala.traits

/**
 * Created by amoszhou on 15/7/1.
 */

/**
 * 4.4中描述的问题，在IDE中写代码时是不存在的，因为编译器会自动编译相关的类
 */
object ScalaMain extends App {
  val foo: Foo = new Main
  println(foo.someMethod())
  println(foo.newMethod())
}
