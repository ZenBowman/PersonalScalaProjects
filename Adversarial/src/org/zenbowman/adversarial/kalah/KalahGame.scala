package org.zenbowman.adversarial.kalah

import org.zenbowman.adversarial.{Action, Game}
import collection.mutable

object KalahGame extends Game[KalahState] {

  def possibleTurns(state: KalahState, turn: Int): List[(List[Int], KalahState)] = {
    if (state.whoseTurn != turn) {
      return List()
    }
    var moveList: List[(List[Int], KalahState)] = List()
    val immediateMoves = Kalah.validMoves(state)
    for (immediateMove <- immediateMoves) {
      val nextState = Kalah.nextStateForMove(state, immediateMove)
      val possTurns = possibleTurns(nextState, turn)
      if (possTurns.isEmpty) {
        moveList = (List(immediateMove), nextState) :: moveList
      } else {
        for ((moves, finalState) <- possTurns) {
          moveList = (immediateMove :: moves, finalState) :: moveList
        }
      }
    }
    moveList
  }

  override def getSuccessors(state: KalahState) = {
    val buffer = new mutable.ListBuffer[KalahState]
    for ((_, successor) <- getSuccessorsExtended(state)) {
      buffer += successor
    }
    buffer.toList
  }

  def getSuccessorsExtended(state: KalahState): List[(List[Int], KalahState)] = {
    possibleTurns(state, state.whoseTurn)
  }

  def exhaustedSeeds(state: KalahState, m: Range): Boolean = {
    for (x <- m; v = state.positions(x) if v > 0) {
      return false
    }
    true
  }

  def isTerminal(state: KalahState) = {
    exhaustedSeeds(state, Kalah.player1Positions) || exhaustedSeeds(state, Kalah.player2Positions)
  }

  def actionForSuccessor(currentState: KalahState, successor: KalahState): Option[Action] = {
    for ((moves, endstate) <- getSuccessorsExtended(currentState)) {
      if (endstate == successor) {
        return Some(new KalahAction(moves))
      }
    }
    None
  }

  def draw(state: KalahState) {}
}
