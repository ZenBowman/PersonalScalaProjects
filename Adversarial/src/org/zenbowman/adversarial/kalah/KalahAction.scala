package org.zenbowman.adversarial.kalah

import org.zenbowman.adversarial.Action

class KalahAction(val moves: List[Int]) extends Action {
  def movesAsIntegerList = moves.asInstanceOf[java.util.List[java.lang.Integer]]
}
