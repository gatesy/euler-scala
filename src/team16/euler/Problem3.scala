package team16.euler

/**
 * Largest prime factor
 * Problem 3
 * 
 * The prime factors of 13195 are 5, 7, 13 and 29.
 * 
 * What is the largest prime factor of the number 600851475143 ?
 */

import scala.collection.mutable.MutableList

object Problem3 extends App {
  val num = 600851475143L
  //val num = 49L
  println("Number = " + num)
  
  var primes = MutableList[Long]()
  var factors = MutableList[Long]()
  var remainder = num
  var i = 2L
  
  while(remainder > 1) {
    if(isPrime(primes, i)) {
      primes += i
      if(isFactor(remainder, i)) {
        factors += i
        remainder = remainder / i
      } else i += 1L
    } else i += 1L
  }
  
  println("Reminader = " + remainder)
  println("Primes = " + primes)
  println("Factors = " + factors)
  
  def isFactor(n: Long, x: Long) = n % x == 0
  def isPrime(primes: MutableList[Long], x: Long) = {
    val result = primes.find(p => x == p && isFactor(x, p))
    result == None || result == Some(x)
  }
}