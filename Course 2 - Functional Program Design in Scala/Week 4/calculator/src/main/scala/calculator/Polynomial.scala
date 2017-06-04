package calculator

object Polynomial {

    def computeDeltaOnFly(a: Signal[Double], b: Signal[Double],
                          c: Signal[Double]): Double = {
        val aCalculated = a()
        val bCalculated = b()
        val cCalculated = c()

        (bCalculated * bCalculated) - (4 * aCalculated * cCalculated)
    }

    def computeSolutionOnFly(a: Signal[Double], b: Signal[Double],
                         c: Signal[Double], delta: Signal[Double]): Set[Double] = {
        val aCalculated = a()
        val bCalculated = b()
        val cCalculated = c()

        val deltaComputed = delta()

        val twoTimesA = (2 * aCalculated)
        val sqrtDelta = Math.sqrt(deltaComputed)

        val first  = ((-1 *  bCalculated) + (sqrtDelta)) / twoTimesA
        val second = ((-1 *  bCalculated) - (sqrtDelta)) / twoTimesA
        Set(first, second)
    }




    def computeDelta(a: Signal[Double], b: Signal[Double],
                     c: Signal[Double]): Signal[Double] = {
        Signal(computeDeltaOnFly(a,b,c))
    }

    def computeSolutions(a: Signal[Double], b: Signal[Double],
                         c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
        Signal(computeSolutionOnFly(a, b, c, delta))
    }
}
