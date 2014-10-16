package team16.euler

/**
 * Sum square difference
 * Problem 6
 * 
 * The sum of the squares of the first ten natural numbers is,
 *    1^2 + 2^2 + ... + 10^2 = 385
 * The square of the sum of the first ten natural numbers is,
 *    (1 + 2 + ... + 10)^2 = 552 = 3025
 * Hence the difference between the sum of the squares of the first ten natural numbers 
 * and the square of the sum is 3025 − 385 = 2640.
 * 
 * Find the difference between the sum of the squares of the first one hundred natural numbers
 * and the square of the sum.
 */

object Problem6 extends App {
  val nums = 1 to 100
  val squares = nums map { x => x*x }
  val sumOfSquares = squares sum
  val squareOfSum = { val temp = nums sum; temp * temp }
  val difference = squareOfSum - sumOfSquares
  
  println("Sum of squares " + sumOfSquares + " of " + squares)
  println("Square of sum " + squareOfSum + " of " + nums)
  println("Difference = " + difference)
  
}