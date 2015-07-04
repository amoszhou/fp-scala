package fpdesign

/**
 * Created by amoszhou on 15/7/2.
 */
class ChooseDataType {

}


object ChooseDataType {

  def sum(ints: IndexedSeq[Int]): Int = {
    if (ints.size <= 1)
      ints.headOption.getOrElse(0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum(l) + sum(r)
    }
  }
}
