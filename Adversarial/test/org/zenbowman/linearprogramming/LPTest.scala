package org.zenbowman.linearprogramming

import org.scalatest.{BeforeAndAfter, FlatSpec}


class LPTest extends FlatSpec with BeforeAndAfter {

  def genProblem1: LPTable = {
    val coefficientRow = new LPRow(List(6, 5, 0, 0))
    val row1 = new LPRow(List(1, 1, 1, 0, 5))
    val row2 = new LPRow(List(3, 2, 0, 1, 12))
    val rowSet = new LPRowSet(List(row1, row2))
    new LPTable(coefficientRow, rowSet, List(2, 3))
  }

  "A table" should "be created" in {
    val lpProblem = genProblem1
    assert(lpProblem != null)
  }

  "Cj-Zj" should "be correctly generated" in {
    val lpProblem = genProblem1
    val result = lpProblem.generateCjMinusZj
    expectResult(List(6, 5, 0, 0))(result)
  }

  "The correct pivot column" should "be generated" in {
    val lpProblem = genProblem1
    val pivCol = lpProblem.selectPivotColumn
    expectResult(Some(0))(pivCol)
  }

  "The correct pivot row" should "be generated" in {
    val lpProblem = genProblem1
    val pivCol = lpProblem.selectPivotColumn
    val pivRow = lpProblem.selectPivotRow(pivCol.get)
    expectResult(1)(pivRow)
  }
}
