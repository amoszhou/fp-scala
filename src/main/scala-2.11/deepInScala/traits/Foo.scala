package deepInScala.traits

/**
 * Created by amoszhou on 15/7/1.
 */
trait Foo {

  def someMethod(): Int = 5
  def newMethod() : String = "Hi"
}

class Main extends Foo

