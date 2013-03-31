package org.zenbowman.adversarial.kalah

import org.zenbowman.adversarial.GameState

class KalahState(val positions: List[Int], val whoseTurn: Int) extends GameState {
  if (positions.size != 14) {
    throw new Exception("Incorrect list size for KalahState, must be a 14 element list")
  }

  def numSeeds(position: Int): Int = positions(position)

  override def equals(obj: Any) = {
    obj match {
      case x: KalahState => (positions == x.positions) && (whoseTurn == x.whoseTurn)
      case _ => false
    }
  }

  // Assumption: Computer is PLAYER 2
  override def getUtility(): Int = {
    if (KalahGame.isTerminal(this)) {
      return positions(Kalah.player2SeedStore) - positions(Kalah.player1SeedStore)
    }

    var acc = 0
    for (x <- Kalah.player2Positions) {
      acc = acc + positions(x)
    }
    for (y <- Kalah.player1Positions) {
      acc = acc - positions(y)
    }
    acc + (2 * positions(Kalah.player2SeedStore)) - (2 * positions(Kalah.player1SeedStore))
  }

  override def toString = {
    "%s, %s".format(whoseTurn, positions)
  }
}

