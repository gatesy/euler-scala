package team16.euler

/**
 * Even Fibonacci numbers
 * Problem 2
 * 
 * Each new term in the Fibonacci sequence is generated by adding the previous two terms. 
 * By starting with 1 and 2, the first 10 terms will be:
 *      1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
 * 
 * By considering the terms in the Fibonacci sequence whose values do not exceed four million, 
 * find the sum of the even-valued terms.
 */

object Problem2 extends App {
  val sum = sumEvenFib(4000000)
  println(sum)
  
  def sumEvenFib(limit : Int) = {
    var a = 1
    var b = 2
    var sum = 2
    
    while(b < limit) {
      val temp = a + b
      a = b
      b = temp
      
      if(even(b)) sum += b
    }
    sum
  }
  
  def even(x : Int) = x % 2 == 0
  
  def fibSeq() = {
    
  }
}