package inclass9


object luckyclass extends App {
  val constantDraw = new luckydraw with ConstantRandom
  val incrDraw = new luckydraw with IncrementRandom
  val pseudoDraw = new luckydraw with PseudoRandom with SeedProvider {
    val RandomSeed = 1409
  }

  val numLucky = 5

  println("constantDraw:")
  constantDraw.showLuckyNumbers(numLucky)
  println("incrDraw:")
  incrDraw.showLuckyNumbers(numLucky)
  println("pseudoDraw:")
  pseudoDraw.showLuckyNumbers(numLucky)
}