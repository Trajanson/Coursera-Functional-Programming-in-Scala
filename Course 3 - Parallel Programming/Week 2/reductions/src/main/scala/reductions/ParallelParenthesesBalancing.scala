package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` if the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var count = 0

    for {
        char <- chars
    } yield {
        if (count < 0) return false
        else if (char == '(') count += 1
        else if (char == ')') count -= 1
    }

    count == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, countLeft: Int, countRight: Int): (Int, Int) = {
      if (idx >= until) (countLeft, countRight)
      else {
          chars(idx) match {
              case '(' => traverse(idx + 1, until, countLeft + 1, countRight)
              case ')' => {
                  if (countLeft > 0) traverse(idx + 1, until, countLeft - 1, countRight)
                  else traverse(idx + 1, until, countLeft, countRight + 1)
              }
              case char => traverse(idx + 1, until, countLeft, countRight)
          }
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      val length = until - from

      if (length > threshold) {
          val rightStartIndex = from + (length / 2)
          val (leftTree, rightTree) = parallel(
              reduce(from, rightStartIndex),
              reduce(rightStartIndex, until)
          )

          val (leftCountLeft, leftCountRight)   = leftTree
          val (rightCountLeft, rightCountRight) = rightTree

          if (leftCountLeft < 0 && rightCountLeft > 0) {
              (leftCountLeft, leftCountRight + rightCountLeft + rightCountRight)
          }
          else if (leftCountRight > 0 && rightCountLeft < 0) {
              (leftCountLeft + leftCountRight + rightCountLeft, rightCountRight)
          } else {
              (leftCountLeft + rightCountLeft, leftCountRight + rightCountRight)
          }

      } else {
          traverse(from, until, 0, 0)
      }

    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
