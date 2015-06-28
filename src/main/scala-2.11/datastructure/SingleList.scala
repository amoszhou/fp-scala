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

  def dropWhile[A](l: List[A], f: A => Boolean): List[A]= l match {
    case Nil => Nil
    case Cons(head,tail) if(f(head)) => dropWhile(tail,f)
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

  val list = List(1, 2, 3, 4, 5, 6, 7)
  println(List.tail(list))

  val list2 = Nil
  println(List.tail(list2))

  val list3 = List(1)
  println(List.tail(list3))

  val afterDrop = List.drop(list,2)
  println(afterDrop)

}