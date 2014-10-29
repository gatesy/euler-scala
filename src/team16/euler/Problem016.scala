package team16.euler

/**
 * Power digit sum
 * Problem 16
 * 
 * 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
 * 
 * What is the sum of the digits of the number 2^1000?
 */

object Problem016 extends App {

  val num = BigInt(2) pow 1000
  println(num)
  
  val digits = num toString() map { _.asDigit }
  println(digits)
  
  println("Sum of digits = " + (digits sum))
}