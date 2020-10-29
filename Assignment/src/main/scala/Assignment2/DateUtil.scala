package Assignment2

object DateUtil extends App {

  type Date = (Int, Int, Int)
  val month = Map(
    (1, "January"),
    (2, "February"),
    (3, "March"),
    (4, "April"),
    (5, "May"),
    (6, "June"),
    (7, "July"),
    (8, "August"),
    (9, "September"),
    (10, "October"),
    (11, "November"),
    (12, "December")
  )

  val days = Map(
    (1, 31),
    (2, 28),
    (3, 31),
    (4, 30),
    (5, 31),
    (6, 30),
    (7, 31),
    (8, 31),
    (9, 30),
    (10, 31),
    (11, 30),
    (12, 31)
  )

  val leapday = Map(
    (1, 31),
    (2, 29),
    (3, 31),
    (4, 30),
    (5, 31),
    (6, 30),
    (7, 31),
    (8, 31),
    (9, 30),
    (10, 31),
    (11, 30),
    (12, 31)
  )

  def isOlder(x: Date, y: Date): Boolean =
    if (isReasonableDate(x) && isReasonableDate(y)) {
      if (x._2 < y._2) true
      else if (x._3 < y._3) {
        if (x._3 == y._3)  true
        else if (x._2 == y._2) {
          if (x._1 < y._1) true
          else false
        } else false
      } else false
    } else false

  def numberInMonth(xs: List[Date], month: Int): Int = {
    def helperfunc(xs: List[Date], month: Int, acc: Int): Int =
      xs match {
        case Nil => acc
        case head :: tail if head._2 == month => helperfunc(tail, month, acc + 1)
        case head :: tail => helperfunc(tail, month, acc)
      }
    if (!(0 < month && month < 13)) 0
    else helperfunc(xs, month, 0)
  }

  def numberInMonths(xs: List[Date], months: List[Int]): Int = {
    def helperfunc(xs: List[Date], month: List[Int], newmonth: List[Int], currmonth: Int, acc: Int): Int = xs match {
      case Nil => acc
      case head :: tail if head._2 == currmonth => helperfunc(tail, month, month.tail, month.head, acc + 1)
      case _ :: tail =>
        newmonth match {
          case Nil => helperfunc(tail, month, month.tail, month.head, acc)
          case headMonth :: tailMonth => helperfunc(xs, month, tailMonth, headMonth, acc)
        }
    }

    if (months.isEmpty) 0
    else helperfunc(xs, months, months.tail, months.head, 0)
  }

  def datesInMonth(xs: List[Date], month: Int): List[Date] = {
    def helperfunc(xs: List[Date], month: Int, ans: List[Date]): List[Date] = xs match {
        case Nil => ans
        case head :: tail if head._2 == month => helperfunc(tail, month, ans :+ head)
        case _ :: tail => helperfunc(tail, month, ans)
      }

    if (!(0 < month && month < 13)) List()
    else helperfunc(xs, month, List())
  }

  def datesInMonths(xs: List[Date], months: List[Int]): List[Date] = {
    def helperfunc(xs: List[Date], months: List[Int], monthToRead: List[Int], monthReading: Int, ans: List[Date]): List[Date] = xs match {
      case Nil => ans
      case head :: tail if head._2 == monthReading => helperfunc(tail, months, months.tail, months.head, ans :+ head)
      case _ :: tail => monthToRead match {
        case Nil => helperfunc(tail, months, months.tail, months.head, ans)
        case headMonth :: tailMonth => helperfunc(xs, months, tailMonth, headMonth, ans)
        }
    }

    if (months.isEmpty) List()
    else helperfunc(xs, months, months.tail, months.head, List())
  }

  def dateToString(d: Date): String = {
    month.get(d._2).fold("")(_ + "") + "-" + d._1 + "-" + d._3
  }

  def whatMonth(n: Int, yr: Int): Int = {
    def helperfunc(n: Int, yr: Int, monthDay: Map[Int, Int], days: Int, month: Int): Int = {
      if (n < days || month == 12) month
      else
        helperfunc(n,
          yr,
          monthDay,
          days + monthDay.get(month).fold(0)(_ + 0),
          month + 1)
    }

    if ((yr % 400 == 0) ||
      (yr % 4 == 0 && yr % 100 != 0) && n < 367) {
      //leap year
      helperfunc(n, yr, leapday, leapday.getOrElse(1, 31), 1)
    } else if (n < 366 && yr > 0) {
      helperfunc(n, yr, days, days.getOrElse(1, 31), 1)
    } else {
      throw new RuntimeException("Day and year is not reasonable")
    }
  }

  def oldest(dates: List[Date]): Option[Date] = {
    def helperfunc(dates: List[Date], max: Date): Option[Date] = {
      dates match {
        case Nil => Some(max)
        case head :: tail if isOlder(head, max) => helperfunc(tail, head)
        case _ :: tail => helperfunc(tail, max)
      }
    }
    if (dates.isEmpty) None
    else helperfunc(dates, dates.head)
  }

  def isReasonableDate(d: Date): Boolean = {
    if (d._1 > 31 || d._2 < 1 || d._2 > 12 || d._3 < 1) false
    else if ((d._3 % 400 == 0) || (d._3 % 4 == 0 && d._3 % 100 != 0)) {
      if (d._1 > leapday.get(d._2).fold(30)(_ + 0)) false
      else true
    } else {
      if (d._1 > days.get(d._2).fold(30)(_ + 0)) false
      else true
    }
  }
}