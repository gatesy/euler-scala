package team16.euler

/**
 * Highly divisible triangular number
 * Problem 12
 * 
 * The sequence of triangle numbers is generated by adding the natural numbers. So the 7th 
 * triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:
 *    1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
 * Let us list the factors of the first seven triangle numbers:
 *  1: 1
 *  3: 1,3
 *  6: 1,2,3,6
 *  10: 1,2,5,10
 *  15: 1,3,5,15
 *  21: 1,3,7,21
 *  28: 1,2,4,7,14,28
 * We can see that 28 is the first triangle number to have over five divisors.
 * 
 * What is the value of the first triangle number to have over five hundred divisors?
 */

import scala.collection._

object Problem012 extends App {

  val triangular: Stream[Long] = {
    def loop(nextNumber: Long, count: Long) : Stream[Long] = nextNumber #:: loop(nextNumber + count, count + 1)
    loop(1, 2)
  }
  
  // Speed this up by having a lookup table. It's far too slow without it
  def countDivisors(number: Long): Int = {
    val start: Long = number + 1L / 2L
    val divisors: Int = ((start to 1L by -1L) filter { number % _ == 0}).length
    if(divisors > 100) println(number + ": " + divisors)
    divisors
  }
  
  def countDivisorsN(lookupTable: mutable.Map[Long, List[Long]], number: Long) = {
    f(getPrimeFactors(lookupTable, number))
  }
  
  //def numberWithOver(divisors: Int) = (triangular takeWhile { x => !(countDivisors(x) > divisors) } takeRight 1).head
  //println(numberWithOver(4))
  
  def getPrimeFactors(lookupTable: mutable.Map[Long, List[Long]], number: Long) : List[Long] = {
    def getPrimeFactors(number: Long): List[Long] = {
      if (lookupTable contains number) lookupTable(number)
      else {
        val firstFactor = (2L to number / 2L) find { number % _ == 0 }
        if(firstFactor == None) List[Long]()
        else firstFactor.get :: getPrimeFactors(number / firstFactor.get)
      }
    }
    val factors = getPrimeFactors(number)
    lookupTable(number) = factors
    factors
  }
  
  // Given a list of prime factors, calculate the number of divisors.
  // This is equal to the powers of each factor (plus 1) multiplied together.
  def f(factors: List[Long]) = {
    var accumulator = mutable.Map[Long, Int]()
    factors foreach { x:Long => accumulator(x) = accumulator.getOrElse(x, 0) + 1 }
    
    (accumulator values).foldLeft(1) { (total, x) => total * (x+1) }
  }
  
  def numberWithOver(divisors: Int) = {
    var lookupTable = mutable.Map[Long, List[Long]]();
    triangular takeWhile { x =>
      val divs = countDivisorsN(lookupTable, x)
      println(x + " --> " + divs)
      divs <= divisors 
    } takeRight 1
  }
  
  //println(f(List(2,2,3)))
  
  //println(getPrimeFactors(27L, Map()))
  
  //var lookupTable = mutable.Map[Long, List[Long]]()
  //(1L to 50000L) foreach { x => println(x + ":-> " + countDivisorsN(lookupTable, x)) }
  
  //triangular take 10 foreach println
  println(numberWithOver(5))
  //println(numberWithOver(500))
  
}