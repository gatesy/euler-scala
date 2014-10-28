package team16.euler

/**
 * Longest Collatz sequence
 * Problem 14
 * 
 * The following iterative sequence is defined for the set of positive integers:
 *    n → n/2 (n is even)
 *    n → 3n + 1 (n is odd)
 * Using the rule above and starting with 13, we generate the following sequence:
 *    13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
 * It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. 
 * 
 * Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
 * 
 * Which starting number, under one million, produces the longest chain?
 * 
 * NOTE: Once the chain starts the terms are allowed to go above one million.
 */

import scala.collection._

object Problem014 extends App {
  val collatz = new Collatz
  //for(i <- 2 to 13) { println(collatz getLength2 i) }
  println(collatz.getLength(13))
  println(collatz.getLength(99996))
  
  for(i <- 1 until 1000000) { collatz getLength i }
  println("Table built")
  println(collatz max)
}

class Collatz {
  private var table = new Array[Int](2); // Starting number -> length
  private val largest = 0
  table(1) = 1
  
  def getLength(start: Int): Int = {
    val chain = buildChain(start, List())
    println(chain)
    updateTable(chain)
    println(table toString)
    val len = table(start)
    len
  }
  
  // Build a chain until we hit a known value.
  @scala.annotation.tailrec
  private def buildChain(from: Int, accumulator: List[Int]): List[Int] = {
    if(table.length > from && table(from) != 0) from :: accumulator
    else buildChain(next(from), from :: accumulator)
  }
  
  // Update the table given a chain.
  @scala.annotation.tailrec
  private def updateTable(chain: List[Int]): Unit = {
    val lastLen = table(chain head)
    val chainTail = chain tail
    
    if(!chainTail.isEmpty) {
      addLookup(chainTail head, lastLen + 1)
      updateTable(chainTail)
    }
  }
  
  private def addLookup(from: Int, length: Int): Unit = {
    while(table.length <= from) {
      growTable
    }
    table(from) = length
  }
  
  private def growTable() = {
    val oldTable = table
    table = new Array[Int](table.length * 2)
    oldTable.copyToArray(table)
  }
  
  def next(from: Int): Int = {
    if(from % 2 == 0) from / 2
    else from * 3 + 1
  }
  
  def max() = {
    println(table)
    //table maxBy { case (x,y) => y }
    table max
  }
  
}