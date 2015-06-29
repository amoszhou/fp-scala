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

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(nx: List[Int]) = foldRight(nx, 1)(_ + _)

  def product2(nx: List[Double]) = foldRight(nx, 1.0)(_ * _)


  /**
   * exercise3.9
   * @param xs
   * @tparam A
   * @return
   */
  def length[A](xs: List[A]): Int = foldRight(xs, 0)((_, acc) => acc + 1)


  /**
   * exercise3.10
   * @param as
   * @param z
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  /**
   * 3.12
   * @param ns
   * @tparam A
   * @return
   */
  def reverse[A](ns: List[A]): List[A] = foldLeft(ns, List[A]())((h: List[A], t: A) => Cons(t, h))

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((t, h) => f(h, t))

  def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a: A) => b => g(f(a, b)))(z)

  def foldRightViaFoldLeft_2[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    val nest = (g: (B => B), a: A) => (b: B) => g(f(a, b))
    val tmp: B => B = foldLeft(l, (b: B) => b)(nest)
    tmp(z)
  }

  @annotation.tailrec
  def startWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startWith(t, t2)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if (startWith(sup, sub)) => true
    case Cons(h, t) => hasSubsequence(t, sub)
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