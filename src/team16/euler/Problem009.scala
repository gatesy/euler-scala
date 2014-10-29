package team16.euler

/**
 * A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
 *    a2 + b2 = c2
 * For example, 32 + 42 = 9 + 16 = 25 = 52.
 * 
 * There exists exactly one Pythagorean triplet for which a + b + c = 1000.
 * Find the product abc.
 */

import Stream._

object Problem009 extends App {

    def getC(a: Int, b: Int) = math.sqrt(a * a + b * b)
    
    def isTriple(a: Int, b: Int, c: Int) = a * a + b * b == c * c
    
    def f(a: Int, target: Int) = {
      val b = Range(a+1, target) find { 
        b => {
          val c = target-(a+b)
          isTriple(a, b, c)
        }
      }
      println(a + " " + b + " " )
      if(b == None) None
      else List(a, b get, target-(a+(b get)))
    }
  
    val target = 1000
    
    // This is bad - we want to generate a sequence and find the first matching triple
    for(a <- 1 to target; b <- a+1 to target) {
      val c = target - (a + b)
      if (isTriple(a,b,c)) println(a + " " + b + " " + c + " product " + a * b * c)
    }
    
    
}