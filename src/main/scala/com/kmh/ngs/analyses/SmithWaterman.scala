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

  def computeSmithWaterman: Int = {
    var i = 0
    while (i < sequence1.size) {
      var j = 0
      while (j < sequence2.size) {
        gap = o + (l - 1) * e
        if (i != 0 && j != 0) {
          if (sequence1(i) == sequence2(j)) {
            // match
            l = 0
            matrix(i)(j) = math.max(0, math.max(
                             matrix(i - 1)(j - 1) + matched, math.max(
                               matrix(i - 1)(j) + gap,
                               matrix(i)(j - 1) + gap)))
          } else {
            // gap
            l += 1
            matrix(i)(j) = math.max(0, math.max(
                             matrix(i - 1)(j - 1) + gap, math.max(
                               matrix(i - 1)(j) + gap,
                               matrix(i)(j - 1) + gap)))
          }
        }
        j += 1
      }
      i += 1
    }

    // find the highest value
    var longest = 0
    var iL = 0
    var jL = 0
    i = 0
    while (i < sequence1.size) {
      var j = 0
      while (j < sequence2.size) {
        if (matrix(i)(j) > longest) {
          longest = matrix(i)(j)
          iL = i
          jL = j
        }
        j += 1
      }
      i += 1
    }

    // Back tract to reconstruct path
    i = iL
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

    val alignOne = new StringBuilder()
    val alignTwo = new StringBuilder()
    
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
  final val MISMATCH = -10
  final val O = -6
  final val L = 0
  final val E = -2

  def apply(sequence1: String, sequence2: String): SmithWaterman = {
    val matrix = Array.fill[Int](sequence1.size + 1, sequence2.size + 1)(0)
    new SmithWaterman("-"+sequence1.replace('N','-'), "-"+sequence2, matrix, MATCH, MISMATCH, O, L, E) 
  }
}
