package org.zenbowman.adversarial.tictactoe

import org.zenbowman.adversarial.GameState


class TicTacToeState(val positions: Grid3x3) extends GameState {
  val XMark = 1
  val OMark = -1
  val Nomark = 0

  def availableSpots() = positions.empties

  def draw() {
    positions.draw()
  }


  override def equals(obj: Any) = {
    obj match {
      case other: TicTacToeState => positions.equals(other.positions)
      case _ => false
    }
  }

  def turn() = {
    var countX = 0
    var countY = 0
    for {
      x <- 0 until 3
      y <- 0 until 3
      p = positions(x, y)
    } {
      if (p == XMark) countX += 1
      if (p == OMark) countY += 1
    }
    if (countX < countY) {
      XMark
    }
    else {
      OMark
    }
  }

  def getUtility(): Int = {
    findLines()
  }

  def findLines(): Int = {
    findVerticalLines().getOrElse(
      findHorizontalLines().getOrElse(
        findDiagonalLine(x => x).getOrElse(
          findDiagonalLine(x => 2 - x).getOrElse(0))))
  }

  def findDiagonalLine(f: Int => Int): Option[Int] = {
    val firstVal = positions(0, f(0))
    for {
      x <- 0 until 3
      y = f(x)
    } {
      if (positions(x, y) != firstVal) {
        return None
      }
    }
    Some(firstVal)
  }

  def findHorizontalLine(y: Int): Option[Int] = {
    val firstVal = positions(0, y)
    if (firstVal == 0) {
      return None
    }
    for (x <- 0 until 3) {
      if (positions(x, y) != firstVal) {
        return None
      }
    }
    Some(firstVal)
  }

  def findVerticalLine(x: Int): Option[Int] = {
    val firstVal = positions(x, 0)
    if (firstVal == 0) {
      return None
    }
    for (y <- 0 until 3) {
      if (positions(x, y) != firstVal) {
        return None
      }
    }
    Some(firstVal)
  }

  def findLinesByType(f: Int => Option[Int]): Option[Int] = {
    for {
      x <- 0 until 3
      v <- f(x)
    } {
      return Some(v)
    }
    None
  }

  def stateWithAction(action: Int, x: Int, y: Int): TicTacToeState = stateWithAction(action, (x, y))

  def stateWithAction(action: Int, pos: (Int, Int)): TicTacToeState = {
    new TicTacToeState(positions.newGrid(action, pos._1, pos._2))
  }

  def findVerticalLines() = findLinesByType(findVerticalLine)

  def findHorizontalLines() = findLinesByType(findHorizontalLine)
}

