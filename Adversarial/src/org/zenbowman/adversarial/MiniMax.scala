package org.zenbowman.adversarial


class MiniMax[T <: GameState](game: Game[T]) {
  val inf = 99999999

  def decision(state: T): Action = {
    val (_, maxValState) = maxValue(state)
    game.actionForSuccessor(state, maxValState).get
  }

  def maxValue(state: T): (Int, T) = {
    if (game.isTerminal(state)) {
      return (state.getUtility(), state)
    }
    var maxVal = -inf
    var maxValState : Option[T] = None
    for {
      successor <- game.getSuccessors(state)
      (minVal, _) = minValue(successor)
    } {
      if (minVal > maxVal) {
        maxVal = minVal
        maxValState = Some(successor)
      }

    }
    (maxVal, maxValState.get)
  }

  def minValue(state: T): (Int, T) = {
    if (game.isTerminal(state)) {
      return (state.getUtility(), state)
    }
    var minVal = inf
    var minValState: Option[T] = None
    for {
      successor <- game.getSuccessors(state)
      (maxVal, _) = maxValue(successor)
    } {
      if (maxVal < minVal) {
        minVal = maxVal
        minValState = Some(successor)
      }
    }
    (minVal, minValState.get)
  }
}
