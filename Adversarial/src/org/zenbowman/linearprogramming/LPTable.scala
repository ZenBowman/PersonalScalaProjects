package org.zenbowman.linearprogramming


class LPTable(val coefficientRow: LPRow, val rowSet: LPRowSet, val baseVars: List[Int]) {

  def generateCjMinusZj: List[Double] = {
    val resultBuffer = coefficientRow.values.toBuffer
    for (i <- 0 until resultBuffer.length) {
      for (bv <- 0 until baseVars.length) {
        val correspondingRow = rowSet.rowList(bv)
        val coefficient = coefficientRow.values(baseVars(bv))
        resultBuffer(i) = resultBuffer(i) - (coefficient * correspondingRow.values(i))
      }
    }
    resultBuffer.toList
  }

  def selectPivotColumn: Option[Int] = {
    val row = generateCjMinusZj
    var max = 0.0
    var colMax: Option[Int] = None
    for (i <- 0 until row.length; v = row(i)) {
      if (v > max) {
        max = v
        colMax = Some(i)
      }
    }
    colMax
  }

  def selectPivotRow(pivotColumn: Int): Int = {
    var min = 0.0
    var minRow: Option[Int] = None
    for (bv <- 0 until baseVars.length) {
      val correspondingRow = rowSet.rowList(bv)
      val rhs = correspondingRow.values.last
      val candidate = rhs / (correspondingRow.values(pivotColumn))
      if (!minRow.isDefined || (candidate < min)) {
        min = candidate
        minRow = Some(bv)
      }
    }
    minRow.get
  }
}
