object sandbox {

    def pascal(c: Int, r: Int): Int = 
      if ((c < 0) || (c > r)) 0
      else if ((c == 0) || (c == r)) 1
      else pascal(c - 1, r - 1) + pascal(c + 1, r - 1) 
        
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = true
      if (chars.isEmpty) {
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = 5
  
  
}