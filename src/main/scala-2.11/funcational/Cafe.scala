package funcational

import java.time.temporal.TemporalAmount

/**
 * Created by amoszhou on 15/6/24.
 */
class Cafe {

  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = Coffee(19.5)
    (cup, Charge(cc, cup.price))
  }

  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce(_.combine(_)))
  }

}


case class Charge(cc: CreditCard, amount: Double) {

  def combine(other: Charge): Charge = {
    if (cc == other.cc)
      Charge(cc, amount + other.amount)
    else
      throw new Exception("Can't combine charges to different cards")
  }

  def coalesce(charges: List[Charge]): List[Charge] =
    charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList
}

case class Coffee(price: Double)

case class CreditCard(no: String, balance: Double) {

}

object TestRunner extends App{
  val s1 = new SubClass("aa")
  s1.name = "bb"
  s1.sayName()
  s1.saySubName()
}

class ParentClass(var name: String){
  def sayName(): Unit ={
    println(name + " in SuperClass")
  }
}

class SubClass(name: String) extends ParentClass(name){

//  def updateName(newName: String): Unit ={
//    name = newName
//  }

  def saySubName(): Unit ={
    println(name + " in subClass")
  }
}


