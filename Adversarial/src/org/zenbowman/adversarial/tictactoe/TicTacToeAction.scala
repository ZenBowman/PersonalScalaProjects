package org.zenbowman.adversarial.tictactoe

import org.zenbowman.adversarial.Action

class TicTacToeAction(val position: (Int, Int), move: Int) extends Action {
  override def toString = {
    "Place X on (" + position._1 + "," + position._2 + ")"
  }
}
