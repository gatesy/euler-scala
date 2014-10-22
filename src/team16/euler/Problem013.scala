package team16.euler

/**
 * Work out the first ten digits of the sum of the following one-hundred 50-digit numbers.
 */

import scala.math.BigInt

object Problem013 extends App {

  def lines = scala.io.Source.fromFile("resources/Problem013.txt") getLines 
  
  def processLines(lines: Iterator[String]) : Iterator[BigInt] = {
    lines map { BigInt(_) }
  }
  
  def sum = processLines(lines) sum
  
  println(sum)
  println(sum toString() take 10)
}