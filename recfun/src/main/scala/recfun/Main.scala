package recfun
import common._

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
   * define what it would look like
   * not find some solution
   */
  def pascal(c: Int, r: Int): Int = if(c == 0 || r==c) 1 else pascal(c-1,r-1) + pascal(c,r-1)

  /**
   * Exercise 2
   * 1.trim out all not "(" or ")" characters
   * 2.recursive reduce all "()" strings
   * 3.if result is empty then true else false
   */
  def balance(chars: List[Char]): Boolean = {
    //1.throw away non ( or ) chars
    val filtered = chars.filter((a)=> ( a.toString=="(" | a.toString==")") )
    if(filtered.length==0) true else{
	    if(filtered.length%2 != 0){
	      false
	    }else{
	      //2.take out adjacent "()" pairs,pass to next
	      val tmpString = filtered.mkString
	      val braceIndex = tmpString.indexOf("()")
	      val nextLoop = filtered.take(braceIndex) ++ filtered.takeRight(filtered.length-braceIndex-2)
	      balance(nextLoop.toList)
	    }
    }
  }

  /**
   * Exercise 3
   * minus myself , pass left to others
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    val sortedCoins = coins.sortWith((a,b)=>a < b)
    //no money , no coin, then 1
    if(money==0) return 1
    if(sortedCoins.length==0) return 0
    //has money and coins
    val head = sortedCoins.head
    val ways = money/head
    if(ways==0) return 0
    var counts = 0
    for(num <- 1 to ways){
       counts += countChange(money-num*head,sortedCoins.tail)
     }
    //plus no extra combines
    counts + countChange(money,sortedCoins.tail)
  }
}
