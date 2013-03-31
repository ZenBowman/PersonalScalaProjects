package org.zenbowman.adversarial

import kalah.{KalahGame, KalahState, Kalah}
import org.scalatest.FlatSpec

class KalahTest extends FlatSpec {

  "A seed" should "be moved to the correct spot" in {
    println(Kalah.player1Positions)
    println(Kalah.player2Positions)
    expect(1)(Kalah.moveSeed(0))
    expect(5)(Kalah.moveSeed(4))
    expect(0)(Kalah.moveSeed(13))
  }

  "The right movable positions" should "be returned for the state" in {
    val initialPositions = List(3, 3, 3, 3, 3, 3, 0, 3, 3, 3, 3, 3, 3, 0)
    val ks1 = new KalahState(initialPositions, 1)
    val ks2 = new KalahState(initialPositions, 2)
    expect(0 until 6)(Kalah.movablePositions(ks1))
    expect(7 until 13)(Kalah.movablePositions(ks2))
  }

  "The right valid moves" should "be returned for the state" in {
    val ks1 = new KalahState(List(3, 0, 3, 0, 3, 0, 3, 0, 2, 2, 2, 2, 2, 0), 1)
    expect(List(0, 2, 4))(Kalah.validMoves(ks1))
  }

  "A selected move" should "be correctly executed" in {
    val ks1 = new KalahState(List(3, 3, 3, 3, 3, 3, 0, 3, 3, 3, 3, 3, 3, 0), 1)
    val ks2 = Kalah.moveSeeds(ks1, 3)
    val expectedks = new KalahState(List(3, 3, 3, 0, 4, 4, 1, 3, 3, 3, 3, 3, 3, 0), 1)
    expect(expectedks)(ks2)
  }

  "Potential turns" should "be generated" in {
    val ks1 = new KalahState(List(3, 3, 3, 3, 3, 3, 0, 3, 3, 3, 3, 3, 3, 0), 1)
    for (successor <- KalahGame.getSuccessors(ks1)) {
      println(successor)
    }
  }
}
