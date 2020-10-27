import scala.collection.mutable.ListBuffer

object Happy extends App {

  def sumOfDigitsSquared(n: Int): Int = {
    var x = n
    var squared = 0
    while (x > 0) {
      var each_num = x % 10;
      squared += each_num * each_num;
      x = x / 10;
    }
    squared
  }

  def isHappy(n: Int): Boolean = {
    var result = sumOfDigitsSquared(n)
    var x = true
    while (result != 1 && result != 4) {
      result = sumOfDigitsSquared(result)
    }
    if (result == 1) x = true
    else if (result == 4) x = false
    else x = false
    x
  }

  def kThHappy(k: Int): Int = {
    var list = new ListBuffer[Int]()
    var count = 0
    var num = 1
    while (count < k) {
      if (isHappy(num)) {
        list += num
        count += 1
      }
      num += 1
    }
    list(list.length - 1)
  }

}