package team16.euler

/**
 * Summation of primes
 * Problem 10
 * 
 * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 * 
 * Find the sum of all the primes below two million.
 */

import Stream._

object Problem10 extends App {

  // This is slow - we should use a sieve and a table
  val p = (2L to 2000000L).foldLeft(List(2L) : Seq[Long])(addIfPrime)
  println(p sum)
  
  def addIfPrime(primes: Seq[Long], x: Long) =
    if (isPrime(primes, x)) primes :+ x else primes
  
  def isPrime(primes: Seq[Long], x: Long) = primes.find(p => x % p == 0L) == None
  
  // Finish implementing this
  def primesTable(max: Int) : Array[Boolean] = {
    var table = new Array[Boolean](max);
    val limit = math.ceil(math.sqrt(max)).toInt
    
    for(i <- 2 until limit) {
      if(table(i)) {
        for(j <- i*i until max by i) {
          table.update(j, false)
        }
      }
    }
    table
  }
  
  // This causes a stack overflow
  def byStream(max: Int) : Long = {
    def primeStream(s: Stream[Long]): Stream[Long] =
      Stream.cons(s.head, primeStream(s.tail filter { _ % s.head != 0 }))
    val primes = primeStream(Stream.iterate(2L) { _ + 1 })
    primes takeWhile { _ < max } sum
    //primes.foldLeft(0L)(_+_)
  }
}