package exercise2_2

/**
  * Implement isSorted, which checks whether an Array[A] is sorted according to a
  * given comparison function.
  */
object SortedCheck {
  /**
    * Check whether array sorted according to given comparison function.
    *
    * @param as Array that is to be checked.
    * @param ordered Comparison function.
    * @tparam A Type of elements in array.
    * @return Boolean showing whether array sorted or not.
    */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n >= as.length) true
      else if (!ordered(as(n - 1), as(n))) false
      else loop(n + 1)
    }

    loop(1)
  }
}
