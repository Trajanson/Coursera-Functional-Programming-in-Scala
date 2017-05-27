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
   def factorial(n: Int, accumulator: Int = 1): Int = {
       if (n <= 1) {
           accumulator
       } else {
           factorial(n - 1, accumulator * n)
       }
   }

   def combination(n: Int, k: Int): Int = {
       val numerator   = factorial(n)
       val denominator = factorial(k) * factorial(n - k)
       numerator / denominator
   }

   def pascal(c: Int, r: Int): Int = {
       combination(r, c)
   }

  /**
   * Exercise 2
   */
   def balance(chars: List[Char], numLeft: Int = 0, numRight: Int = 0): Boolean = {
       if (chars.isEmpty) {
           if (numRight == numLeft) true
           else false
       } else {
           var newNumLeft  = numLeft
           var newNumRight = numRight

           if (chars.head == '(') {
               newNumLeft += 1
           } else if (chars.head == ')') {
               newNumRight += 1
           }

           if (numRight > numLeft) {
               return false
           }

           return balance(chars.tail, newNumLeft, newNumRight)
       }
   }

  /**
   * Exercise 3
   */
   def countChange(money: Int, coins: List[Int]): Int = {
       if (coins.length == 0) {
           return 0
       }
       if (money == 0) {
           return 1
       }

       var memory = Map(0 -> 1)

       coins.foreach((coin: Int) => {

           for (subProblem <- 1 to money) {
               var remainder = subProblem - coin

               if (memory contains remainder) {
                   if (!(memory contains subProblem)) {
                       memory += ((subProblem, 0))
                   }
                   memory += ((subProblem, memory(subProblem) + memory(remainder) ))
               }
           }

       })

       if (memory contains money) {
           return memory(money)
       }

       return 0
   }


}
