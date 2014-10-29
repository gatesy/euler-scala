package team16.euler

/**
 * Smallest multiple
 * Problem 5
 * 
 * 2520 is the smallest number that can be divided by each of the numbers 
 * from 1 to 10 without any remainder.
 * 
 * What is the smallest positive number that is evenly divisible by all of
 * the numbers from 1 to 20?
 */

import Stream._

object Problem005 extends App {
  val factors = for(i <- 20L to 1 by -1) yield i
  println("Factors -> " + factors)
  
  val answer = 232792560L // computed by hand
  println(answer + " is divisble by all? -> " + isDivisibleByAll(answer, factors))
    
  def isDivisibleByAll(n: Long, factors: Seq[Long]) = factors.find(x => !isDivisibleBy(n)(x)) == None
  
  def isDivisibleBy(n: Long)(f: Long) = n % f == 0
  
}