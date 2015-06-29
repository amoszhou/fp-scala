package datastructure

/**
 * Created by amoszhou on 15/6/27.
 */

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(head, tail) => head + sum(tail)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(head, tail) => head * product(tail)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    //    case Cons(head,Nil)=>Nil
    case Cons(head, tail) => tail
  }

  def setHead[A](list: List[A], head: A): List[A] = list match {
    case Nil => Nil
    case Cons(head, tail) => Cons(head, tail)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else drop(tail(l), n - 1)
  }

  /**
   * 从头至尾删除满足条件的元素，一旦碰到不满足的元素，则终止
   * @param l
   * @param f
   * @tparam A
   * @return
   */
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    //    这样写，是Filter功能，而不是dropWhile
    //    case Nil => Nil
    //    case Cons(head, tail) =>
    //      if (f(head)) {
    //        dropWhile(tail, f)
    //      } else {
    //        Cons(head, dropWhile(tail, f))
    //      }
    case Cons(head, tail) if f(head) => dropWhile(tail)(f)
    case _ => l

  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }
}

object dataStructureApp extends App {

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }
  println(x)

  val list = List(8, 1, 2, 3, 4, 5, 6, 7)
  println(List.tail(list))

  val list2 = Nil
  println(List.tail(list2))

  val list3 = List(1)
  println(List.tail(list3))

  val afterDrop = List.drop(list, 2)
  println(afterDrop)

  /**
   * dropWhile Test
   */
  val afterDropWithPredicate = List.dropWhile(list)((x: Int) => x < 4)

  println(afterDropWithPredicate)

  /**
   * scala类库自带的List的dropWhile功能测试测试
   */
  val scalaList = scala.collection.immutable.List(8, 1, 2, 3, 4, 5)
  println(scalaList.dropWhile(_ < 4))
}