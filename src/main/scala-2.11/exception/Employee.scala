package exception

/**
 * Created by amoszhou on 15/6/30.
 */
case class Employee(name: String, department: String) {

  def lookupByName(name: String): Option[Employee] = {
    None
  }

  val joeDepartment: Option[String] = lookupByName("Joe").map(_.department)

  val dept: String = lookupByName("Joe").map(_.dept).
    filter(_ != "Accounting").getOrElse("Default Dept")
}
