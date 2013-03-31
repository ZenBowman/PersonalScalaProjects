package org.zenbowman.adversarial.tictactoe

import org.zenbowman.adversarial.{Game, Action}

object TicTacToe extends Game[TicTacToeState] {
  def draw(state: TicTacToeState) = state.draw()

  def isTerminal(state: TicTacToeState): Boolean = {
    (state.getUtility() != 0) || (state.availableSpots().length == 0)
  }

  def getSuccessors(state: TicTacToeState): List[TicTacToeState] = {
    val whoseTurn = state.turn()
    for (availableMove <- state.availableSpots) yield state.stateWithAction(whoseTurn, availableMove)
  }

  def actionForSuccessor(currentState: TicTacToeState, successor: TicTacToeState): Option[Action] = {
    val whoseTurn = currentState.turn()
    for (availableMove <- currentState.availableSpots()) {
      if (currentState.stateWithAction(whoseTurn, availableMove) == successor)
        return Some(new TicTacToeAction(availableMove, whoseTurn))
    }
    None
  }
}

