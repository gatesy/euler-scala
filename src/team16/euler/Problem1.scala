package team16.euler;

/**
 * Multiples of 3 and 5
 * Problem 1
 * 
 * If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. 
 * The sum of these multiples is 23.
 * 
 * Find the sum of all the multiples of 3 or 5 below 1000.
 */

object Problem1 extends App {
  println("Hello, World!")
  val values = listOfMultiples(List(3,5), 1000)
  val sum = values.foldLeft(0)(_+_)
  println("Sum = " + sum)
  
  def listOfMultiples(multiples: List[Int], limit: Int) = {
    val modN = (x: Int) => (n: Int) => x % n == 0
    for( x <- 1 until limit if multiples.find(modN(x)) != None) yield x
  }
  
}