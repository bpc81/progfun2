package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Var[Double]( b() * b() - 4 * a() * c() )
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    def recompute(a: Double, b: Double, c: Double, delta: Double): Set[Double] = {
      if (delta > 0d) {
        val sqrtDelta = math.sqrt(delta)
        Set(
          (-b + sqrtDelta) / a / 2,
          (-b - sqrtDelta) / a / 2)
      } else if (delta == 0d) Set( -b / a / 2)
      else Set[Double]()
    }
    Var[Set[Double]]( recompute(a(), b(), c(), delta()) )
  }
}
