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
    if (c == 0 || c == r || r == 1) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def check(c: Char, n: Int): Int = {
      if (c == '(') n + 1
      else if (c == ')') n - 1
      else n
    }
    def loop(chars: List[Char], n: Int): Boolean = {
      if (chars.isEmpty) {
        n == 0
      } else {
        val checked_n = check(chars.head, n)
        if (checked_n >= 0) loop(chars.tail, checked_n)
        else false
      }

    }
    loop(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0)
      1
    else if (money > 0 && !coins.isEmpty)
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
    else
      0
}
