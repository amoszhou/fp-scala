package datastructure

/**
 * Created by amoszhou on 15/6/29.
 */
sealed trait Tree[+A] {
  def size(): Int = this match {
    case Leaf(_) => 1
    case Branch(left, right) => left.size() + right.size() + 1 //Don't forget branch itself
  }

  def depth(): Int = this match {
    case Leaf(_) => 1
    //深度是取比较大的那一边的深度
    case Branch(left, right) => (left.depth() max right.depth()) + 1
  }

  def map[B](f: A => B): Tree[B] = this match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(left.map(f), right.map(f))
  }

}

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def maximum(l: Tree[Int]): Int = l match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }
}

