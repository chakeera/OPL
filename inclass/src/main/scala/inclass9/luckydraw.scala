package inclass9

trait RandomnessService {
  def nextRandomNum(): Int
}
trait ConstantRandom extends RandomnessService {
  def nextRandomNum() = 3
}
trait IncrementRandom extends RandomnessService { //I don't know better way to do it
  var num = 3
  def nextRandomNum() = {
    num = num+1
    num
  }
}
trait SeedProvider {
  def RandomSeed: Long
}

trait PseudoRandom extends RandomnessService {
  this: SeedProvider => val RNG = new scala.util.Random(RandomSeed)
  def nextRandomNum() = RNG.nextInt()
}

class luckydraw {
  this: RandomnessService =>
  def showLuckyNumbers(n: Int) = {
    def generateList(n:Int, K:List[Int] => List[Int]): List[Int] = n
    match{
      case 0 => K(Nil)
      case _ => generateList(n-1, (R:List[Int]) => K(n::R))
    }
    val luckyNumbers = generateList(n,(r: List[Int]) => r).map(k => this.nextRandomNum())
    luckyNumbers.foreach(println(_))
  }
}