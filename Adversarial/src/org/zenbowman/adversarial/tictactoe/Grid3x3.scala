package org.zenbowman.adversarial.tictactoe

/**
 * Created with IntelliJ IDEA.
 * User: psamtani
 * Date: 3/17/13
 * Time: 8:50 AM
 * To change this template use File | Settings | File Templates.
 */
class Grid3x3(val positions: List[Int]) {

  override def equals(obj: Any) = {
    obj match {
      case other: Grid3x3 => positions == other.positions
      case _ => false
    }
  }

  def empties(): List[(Int, Int)] = {
    val empty = for {
      x <- 0 until 3
      y <- 0 until 3
      if (apply(x, y) == 0)
    } yield (x, y)
    empty.toList
  }

  def positionIndex(x: Int, y: Int): Int = ((3 * y) + x)

  def newGrid(turn: Int, x: Int, y: Int): Grid3x3 = {
    val thisList = positions.toBuffer
    thisList(positionIndex(x, y)) = turn
    new Grid3x3(thisList.toList)
  }

  def apply(x: Int, y: Int): Int = {
    positions(positionIndex(x, y))
  }

  def draw() {
    println("---------")
    for (y <- 0 until 3) {
      for (x <- 0 until 3) {
        apply(x, y) match {
          case 1 => print("X")
          case -1 => print("O")
          case _ => print(".")
        }
        print("\t")
      }
      println()
    }
    println("---------")
  }
}
