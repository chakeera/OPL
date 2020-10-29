package Assignment2

object OptionFriends extends App {
  def lookup(xs: List[(String, String)], key: String): Option[String] = xs match {
    case Nil => None
    case head :: _ if head._1 == key => Some(head._2)
    case _ :: tail => lookup(tail, key)
  }

  def lookups[A](xs: List[(String, A)], key: String): Option[A] = xs match {
    case Nil => None
    case head :: _ if head._1 == key => Some(head._2)
    case _ :: tail => lookups[A](tail, key)
  }

  def resolve(userIdFromLoginName: String => Option[String], majorFromUserId: String => Option[String], divisionFromMajor: String => Option[String],
              averageScoreFromDivision: String => Option[Double],
              loginName: String): Double = userIdFromLoginName(loginName).flatMap(majorFromUserId).flatMap(majorFromUserId).flatMap(divisionFromMajor).flatMap(averageScoreFromDivision).fold(0.0)(_ + 0)
}