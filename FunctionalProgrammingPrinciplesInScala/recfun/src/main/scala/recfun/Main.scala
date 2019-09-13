package recfun

import scala.annotation.tailrec

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

      if (c == 0 || c == r) 1
      else if (c == 1 || r - c == 1) r
      else {
        @tailrec
        def rowCalc(v: Vector[Int], r: Int): Vector[Int] =
          if (r == 0) v
          else {
            rowCalc((0 +: v, v :+ 0).zipped.map(_ + _), r - 1)
          }

        val row: Vector[Int] = Vector(1)
        rowCalc(row, r)(c)
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      @tailrec
      def balanceRec(chars: List[Char], acc: Int): Boolean = {
        if (acc < 0) false
        else if (chars.isEmpty) acc == 0
        else {
          val incr = if (chars.head == '(') 1 else if (chars.head == ')') -1 else 0
          balanceRec(chars.tail, acc + incr)
        }
      }

      balanceRec(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money < 0 || coins.isEmpty ) 0
      else if (money == 0 ) 1
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
