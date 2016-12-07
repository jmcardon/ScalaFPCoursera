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
 // 1  0
 // 1 1  1
 // 1 2 1  2
 // 1 3 3 1  3
 // 1 4 6 4 1  4
  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
    if (c < 0 || (r < 0 || c > r + 1)) {
      throw new IndexOutOfBoundsException("Index out of the triangle's bounds!!")
    } else {
      if (c == 0) 1
      else {
        if (c != r) {
          pascal(c - 1, r - 1) + pascal(c, r - 1)
        }
        else 1
      }
    }
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def charbalance(chars: List[Char], countcurly1:Int, countcurly2:Int): Boolean =
      if (chars.isEmpty && (countcurly1 != countcurly2)) false
      else if (chars.isEmpty && (countcurly1 == countcurly2)) true
      else if (countcurly2 > countcurly1) false
      else if (chars.head == '(') charbalance(chars.tail, countcurly1+1, countcurly2)
      else if (chars.head == ')') charbalance(chars.tail, countcurly1, countcurly2 + 1)
      else charbalance(chars.tail,countcurly1,countcurly2)
    charbalance(chars,0,0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeSorted(money: Int, coins: List[Int]): Int =
      if (coins.isEmpty) return 0
      else if (money < 0) return 0
      else if (money == 0) return 1
      else countChangeSorted(money - coins.head, coins) + countChange(money, coins.tail)
    countChangeSorted(money,coins.sorted)
  }

  }

