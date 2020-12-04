package inclass2

object Newton extends App {
  def goodEnough(guess: Double) = math.abs(math.log(guess) - 1) < 1e-10

  def improve(guess: Double) = guess - (math.log(guess) - 1) * guess

  def repeat(guess: Double): Double =
    if (goodEnough(guess)) guess else repeat(improve(guess))

  def computeE = repeat(1.0)

  def sqrt(y: Double): Double = {
    def setup(guess: Double): Double = math.pow(guess, 2) - y

    def goodEnoughSqrt(guess: Double) = {
      math.abs(setup(guess)) < 1e-10
    }

    def improveSqrt(guess: Double) = guess - (setup(guess)) / (2 * guess)

    def repeatSqrt(guess: Double): Double =
      if (goodEnoughSqrt(guess)) guess else repeatSqrt(improveSqrt(guess))

    val guess = math.random() * 2
    repeatSqrt(guess)
  }

  var a = computeE
  println(a)
  a = sqrt(9)
  println(a)
  a = sqrt(3)
  println(a)
  a = sqrt(2)
  println(a)
}
