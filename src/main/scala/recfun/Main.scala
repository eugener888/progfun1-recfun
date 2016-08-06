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
    def pascal(c: Int, r: Int): Int =
      if (c == 0 || c == r) 1 else pascal(c, r - 1) + pascal(c - 1, r - 1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceParameter(chars: List[Char], openParanthesesNum: Int): Boolean = {
        if (chars.isEmpty) openParanthesesNum == 0
        else {
          val firstChar = chars.head
          val restChars = chars.tail
          if (firstChar == '(') balanceParameter(restChars, openParanthesesNum + 1)
          else if (firstChar == ')') {
            if (openParanthesesNum == 0) false else balanceParameter(restChars, openParanthesesNum - 1)
          }
          else balanceParameter(restChars, openParanthesesNum)
        }
      }
      balanceParameter(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0 || coins.isEmpty) 0
    else {
      val coinsSorted = coins.sorted
      val firstCoin = coinsSorted.head
      val restCoins = coinsSorted.tail
      val restMoney = money - firstCoin
      if (restMoney < 0) countChange(money, restCoins)
      else if (restMoney == 0) 1
      else countChange(restMoney, coinsSorted) + countChange(money, restCoins)
    }
  }
}
