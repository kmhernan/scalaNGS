package com.kmh.ngs.analyses
import scala.collection.mutable.ArrayStack
import scala.math

/**
 * Implementation of the Smith-Waterman local sequence alignment
 * algorithm
 */
class SmithWaterman(
	sequence1: String,
	sequence2: String,
        matrix: Array[Array[Int]],
        matched: Int,
        mismatch: Int,
        o: Int,
        var l: Int,
        e: Int) {

  var gap = 0
  var alignments = Array.fill[String](2)(null)
  val alignOne = new StringBuilder()
  val alignTwo = new StringBuilder()

  def computeSmithWaterman: Int = {
    var i = 1
    while (i <= sequence1.size) {
      var j = 1
      while (j <= sequence2.size) {

        val scoreDiag = matrix(i - 1)(j - 1) + weight(i, j) 
        val scoreLeft = matrix(i)(j - 1) - 1
        val scoreUp = matrix(i - 1)(j) - 1
        matrix(i)(j) = math.max(math.max(math.max(scoreDiag, scoreLeft), scoreUp), 0)
        j += 1
      }
      i += 1
    }

    // Backtrack
    i = 1
    var j = 1
    var max = matrix(i)(j)
    
    var k = 1
    while (k <= sequence1.size) {
      var l = 1
      while (l <= sequence2.size) {
        if (matrix(k)(l) > max) {
          i = k
          j = l
          max = matrix(k)(l)
        }
        l += 1
      }
      k += 1
    }

    var mScore= matrix(i)(j)

    k = sequence1.length
    l = sequence2.length


    while (k > i) {
      alignTwo.append("-")
      alignOne.append(sequence1(k - 1))
      k -= 1 
    }
    while (l > j) {
      alignOne.append("-")
      alignTwo.append(sequence2(l - 1))
      l -= 1
    }
  
  private def processMatrix(i: Int, j: Int): (Int, Int) = 
    matrix(i)(j) match {
      case 0 => (i, j)
      case e: _ =>  
    while (matrix(i)(j) != 0) {
      if (matrix(i)(j) == matrix(i - 1)(j - 1) + weight(i, j)) {
        alignOne.append(sequence1(i-1))
        alignTwo.append(sequence2(j-1))
        i -= 1
        j -= 1
      }
      else if (matrix(i)(j) == matrix(i)(j-1) - 1) {
        alignOne.append("-")
        alignTwo.append(sequence2(j-1))
        j -= 1
      }
      else {
        alignOne.append(sequence1(i-1))
        alignTwo.append("-")
        i -= 1
      }
    }

    while (i > 0) {
      alignOne.append("-")
      alignTwo.append(sequence1(i - 1))
      i -= 1
    }
    while (j > 0) {
      alignOne.append("-")
      alignTwo.append(sequence2(j - 1))
      j -= 1
    }
    
    alignments = Array(alignOne.toString.reverse, alignTwo.toString.reverse)
    return mScore
    // Back tract to reconstruct path
    /*i = iL
    var j = jL
    val actions = new ArrayStack[String]()

    while (i != 0 && j != 0) {
      // diag case
      if (math.max(matrix(i - 1)(j - 1),
            math.max(matrix(i - 1)(j), matrix(i)(j - 1))) == matrix(i - 1)(j - 1)) {
        actions.push("align")
        i = i - 1
        j = j - 1
        // left case
      } else if (math.max(matrix(i - 1)(j - 1),
                   math.max(matrix(i - 1)(j), matrix(i)(j - 1))) == matrix(i)(j - 1)) {
        actions.push("insert")
        j = j - 1
        //up case
      } else {
        actions.push("delete")
        i = i - 1
      }
    }
    
    val backActions = actions.clone()
    var z = 0
    while (z < sequence1.size) {
      alignOne.append(sequence1(z))
      if (!actions.isEmpty) {
        val curAction = actions.pop()
        if (curAction == "insert") {
          alignOne.append("-")
          while (!actions.isEmpty && actions.top == "insert") {
            alignOne.append("-")
            actions.pop()
          }
        }
      }
      z += 1
    }

    z = 0
    while (z < sequence2.size) {
      alignTwo.append(sequence2(z))
      if (!backActions.isEmpty) {
        val curAction = backActions.pop()
        if (curAction == "delete") {
          alignTwo.append("-")
          while (!backActions.isEmpty && backActions.top == "delete") {
            alignTwo.append("-")
            backActions.pop()
          }
        }
      }
      z += 1
    }

    alignments = Array(alignOne.toString, alignTwo.toString)
    return longest
    */
  }

  private def weight(i: Int, j: Int): Int = {
    if (sequence1(i-1) == sequence2(j-1)) 2 
    else -1 
  }

  def printMatrix: Unit = {
    var i = 0
    while (i < sequence1.size) {
      if (i == 0) {
        var z = 0
        while (z < sequence2.size) {
          if (z == 0) print("   ")
          print(sequence2(z) + "  ") 
          if (z == sequence2.size - 1) println()
          z += 1
        }
      }
      var j = 0
      while (j < sequence2.size) {
        if (j == 0) print (sequence1(i) + "  ")
        print(matrix(i)(j) + "  ")
        j += 1
      }
      println()
      i += 1
    }
    println()
  } 
}

/**
 * Companion object
 */
object SmithWaterman {
  final val MATCH = 6
  final val MISMATCH = -15
  final val O = -2
  final val L = 0
  final val E = -1

  def apply(sequence1: String, sequence2: String): SmithWaterman = {
    //val matrix = Array.fill[Int](sequence1.size + 1, sequence2.size + 1)(0)
    val matrix = Array.ofDim[Int](sequence1.size + 1, sequence2.size + 1)
    new SmithWaterman(sequence1.replace('N','-'), sequence2, matrix, MATCH, MISMATCH, O, L, E) 
  }
}
