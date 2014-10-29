package team16.euler

/**
 * 10001st prime
 * Problem 7
 * By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
 * 
 * What is the 10,001st prime number?
 */

import Stream._

object Problem007 extends App {
  
  def isPrime(primes: List[Long], x: Long) = primes.find(p => x % p == 0L) == None
  
  def addIfPrime(primes: List[Long], x: Long) = {
    if(isPrime(primes, x)) primes :+ x
    else primes
  }

  def addNextPrime(primes: List[Long], from: Long) = {
    primes :+ ( iterate(from) { i => i + 1 } find(i => isPrime(primes, i)) get)
  }
  
  val n = 10001
  var primes = List[Long](2)
  
  while(primes.length < n) {
    primes = addNextPrime(primes, primes.last)
  }
  
  println(primes last)
  
}