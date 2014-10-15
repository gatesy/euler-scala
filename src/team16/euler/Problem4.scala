package team16.euler

/**
 * Largest palindrome product
 * Problem 4
 * 
 * A palindromic number reads the same both ways. The largest palindrome made from 
 * the product of two 2-digit numbers is 9009 = 91 × 99.
 * 
 * Find the largest palindrome made from the product of two 3-digit numbers.
 */

object Problem4 extends App {
  val numbers = for(i <- 999L to 100L by -1; j <- 999L to 100L by -1) yield i*j
  val numpal = (numbers filter(x => isPalindrome(x.toString toList))).max
  println(numpal)
  
  def isPalindrome(s: List[Char]) : Boolean = {
    if(s.length < 2) true
    else if(s.head == s.last) isPalindrome(s slice(1,(s length)-1))
    else false
  }
  
}