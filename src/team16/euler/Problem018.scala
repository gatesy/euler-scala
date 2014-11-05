package team16.euler

/**
 * Maximum path sum I
 * Problem 18
 * 
 * By starting at the top of the triangle below and moving to adjacent numbers on the row below, 
 * the maximum total from top to bottom is 23.
 * 
 * 3
 * 7 4
 * 2 4 6
 * 8 5 9 3
 * 
 * That is, 3 + 7 + 4 + 9 = 23.
 * 
 * Find the maximum total from top to bottom of the triangle below:
 * 
 * 75
 * 95 64
 * 17 47 82
 * 18 35 87 10
 * 20 04 82 47 65
 * 19 01 23 75 03 34
 * 88 02 77 73 07 63 67
 * 99 65 04 28 06 16 70 92
 * 41 41 26 56 83 40 80 70 33
 * 41 48 72 33 47 32 37 16 94 29
 * 53 71 44 65 25 43 91 52 97 51 14
 * 70 11 33 28 77 73 17 78 39 68 17 57
 * 91 71 52 38 17 14 91 43 58 50 27 29 48
 * 63 66 04 68 89 53 67 30 73 16 69 87 40 31
 * 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
 * 
 * NOTE: As there are only 16384 routes, it is possible to solve this problem by trying every route. 
 * However, Problem 67, is the same challenge with a triangle containing one-hundred rows; it cannot 
 * be solved by brute force, and requires a clever method! ;o)
 */

import scala.collection._

object Problem018 extends App {
  val a = List[Int](3, 7, 4, 2, 4, 6, 8, 5, 9, 3)
  for(i <- 1 to 4) {
    val idx = Triangle.rowRange(i)
    println(a.slice(idx._1, idx._2))
  }
  for(i <- 1 to 4) println(Triangle.rowRange(i))
  
  println(Triangle.row(5))
  println(Triangle.children(4))
  
  val t = new Triangle(a)
}

object Triangle {
  def rowRange(row: Int): (Int, Int) = {
    val rowMinus1 = row * (row-1) / 2
    (rowMinus1, rowMinus1 + row)
  }
  
  def row(index: Int): Int = {
    def rowRec(index: Int, currentRow: Int): Int = {
      val range = rowRange(currentRow)
      if(range._1 <= index && range._2 > index) currentRow
      else rowRec(index, currentRow + 1)
    }
    rowRec(index, 1)
  }
  
  def children(index: Int): (Int, Int) = {
    val rowNum = row(index)
    val rowStart = rowRange(rowNum)._1
    val nextRowStart = rowRange(rowNum+1)._1
    val pos = index - rowStart
    (nextRowStart+pos, nextRowStart+pos+1)
  }
}

class Triangle(values: List[Int]) {
  private val data = values map { new Data(_, 0) }
  println(Triangle.rowRange(Triangle.row(data.length-1)))
}

class Data(value: Int, maxSum: Int) {
  
}

