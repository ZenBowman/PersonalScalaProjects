package org.zenbowman.adversarial


class MiniMax[T <: GameState](game: Game[T]) {
  val inf = 99999999
  var count = 0

  def decision(state: T): Action = {
    count = 0
    val (utility, maxValState) = maxValue(state)
    game.actionForSuccessor(state, maxValState).get
  }

  def maxValue(state: T): (Int, T) = {
    if (game.isTerminal(state)) {
      return (state.getUtility(), state)
    }
    var maxVal = -inf
    var maxValState: Option[T] = None
    for {
      successor <- game.getSuccessors(state)
      (minVal, _) = minValue(successor)
    } {
      count = count + 1
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
      count = count + 1
      if (maxVal < minVal) {
        minVal = maxVal
        minValState = Some(successor)
      }
    }
    (minVal, minValState.get)
  }
}

class AlphaBetaSearch[T <: GameState](game: Game[T]) {
  val inf = 99999999
  var count = 0

  def decision(state: T): Action = {
    count = 0
    val (_, maxValState) = maxValue(state, -inf, inf)
    game.actionForSuccessor(state, maxValState).get
  }

  def maxValue(state: T, alpha: Int, beta: Int): (Int, T) = {
    if (game.isTerminal(state)) {
      return (state.getUtility(), state)
    }
    var newAlpha = alpha
    var maxVal = -inf
    var maxValState: Option[T] = None
    for (successor <- game.getSuccessors(state)) {
      val (minVal, _) = minValue(successor, newAlpha, beta)
      count = count + 1
      if (minVal > maxVal) {
        maxVal = minVal
        maxValState = Some(successor)
      }
      if (maxVal >= beta) {
        return (maxVal, maxValState.get)
      }
      newAlpha = math.max(maxVal, newAlpha)
    }
    (maxVal, maxValState.get)
  }

  def minValue(state: T, alpha: Int, beta: Int): (Int, T) = {
    if (game.isTerminal(state)) {
      return (state.getUtility(), state)
    }
    var newBeta = beta
    var minVal = inf
    var minValState: Option[T] = None
    for {
      successor <- game.getSuccessors(state)
    } {
      val (maxVal, _) = maxValue(successor, alpha, newBeta)
      count = count + 1
      if (maxVal < minVal) {
        minVal = maxVal
        minValState = Some(successor)
      }
      if (minVal <= alpha) {
        return (minVal, minValState.get)
      }
      newBeta = math.min(newBeta, minVal)
    }
    (minVal, minValState.get)
  }
}
