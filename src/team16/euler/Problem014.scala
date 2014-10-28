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
  val largest = 1000000
  val collatz = new Collatz(largest)
  
  for(i <- 1 until largest) { collatz getLength i }
  println("Table built")
  println(collatz max)
}

class Collatz(tableSize: Int) {
  private var table = new Array[Int](tableSize); // Starting number -> length
  table(1) = 1
  
  def getLength(start: Int): Int = {
    val chain = buildChain(start, List())
    val lastLen = table(chain.head.toInt)
    /*
    println(chain)
    println(start + " -> " + lastLen + " + " + (chain.length-1) + " = " + (lastLen + chain.length -1))
    */
    updateTable(chain, lastLen)
    table(start)
  }
  
  // Build a chain until we hit a known value.
  @scala.annotation.tailrec
  private def buildChain(from: Long, accumulator: List[Long]): List[Long] = {
    if(table.length > from && table(from.toInt) != 0) from :: accumulator
    else buildChain(next(from), from :: accumulator)
  }
  
  // Update the table given a chain and the sequence length of the head
  @scala.annotation.tailrec
  private def updateTable(chain: List[Long], headLen: Int): Unit = {
    addLookup(chain head, headLen)
    val chainTail = chain tail
    
    if(!chainTail.isEmpty) {
      updateTable(chainTail, headLen + 1)
    }
  }
  
  private def addLookup(from: Long, length: Int): Unit = {
    if(from < table.length) table(from.toInt) = length
  }
    
  def next(from: Long): Long = {
    if(from % 2L == 0) from / 2L
    else from * 3L + 1L
  }
  
  def max() = {
    val r = table.foldLeft((0,0L,0)) { 
      case((max_i,max_len, i) , len) => 
        if(len > max_len) (i,len,i+1) 
        else (max_i, max_len,i+1) }
    (r._1 -> r._2)
  }
  
}