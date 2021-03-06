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
  ) withWarmer (new Warmer.Default)

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

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    def countParenthesis(idx: Int, count: Int): Int = {
      if (count < 0 || idx == chars.length)
        count
      else {
        chars(idx) match {
          case '(' => countParenthesis(idx + 1, count + 1)
          case ')' => countParenthesis(idx + 1, count - 1)
          case _ => countParenthesis(idx + 1, count)
        }
      }

    }
    countParenthesis(0, 0) == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    @tailrec
    def traverse(idx: Int, until: Int, count: Int, weight: Int): (Int, Int) = {
      if (idx >= until)
        (count, weight)
      else {
        chars(idx) match {
          case '(' => traverse(idx + 1, until, count + 1, if (weight == 0) 1 else weight)
          case ')' => traverse(idx + 1, until, count - 1, if (weight == 0) -1 else weight)
          case _ => traverse(idx + 1, until, count, weight)
        }
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold)
        traverse(from, until, 0, 0)
      else {
        val mid = from + (until - from) / 2
        val ((countLeft, weightLeft), (countRight, balanceRight)) =
          parallel(reduce(from, mid), reduce(mid, until))
        (countLeft + countRight, if (weightLeft == 0) balanceRight else weightLeft)
      }
    }

    reduce(0, chars.length) == (0, 1)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
