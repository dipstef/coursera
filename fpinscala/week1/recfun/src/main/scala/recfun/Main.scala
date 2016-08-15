package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == 0 || c == r) {
      1
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def go(left: Int, right: Int, remaining: List[Char]): Boolean = {
      if (remaining.isEmpty)
        left == right
      else {
        remaining.head match {
          case '(' => go(left + 1, right, remaining.tail)
          case ')' => if (left > right) go(left, right + 1, remaining.tail) else false
          case _ => go(left, right, remaining.tail)
        }
      }

    }
    go(0, 0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def go(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty || money < 0) 0
      else if (money == 0) 1
      else go(money - coins.head, coins) + go(money, coins.tail)
    }

    go(money, coins.filter(_ <= money))
  }

}
