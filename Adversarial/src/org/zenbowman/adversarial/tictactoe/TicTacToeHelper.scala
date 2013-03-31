package org.zenbowman.adversarial.tictactoe

import org.zenbowman.adversarial.AlphaBetaSearch

object TicTacToeHelper {

  def initialState() = {
    createTicTacToeState(List(0, 0, 0, 0, 0, 0, 0, 0, 0))
  }

  def createTicTacToeState(pos: List[Int]) = {
    new TicTacToeState(new Grid3x3(pos))
  }

  def promptForCoordinate(prompt: String): Int = {
    Console.println(prompt)
    while (true) {
      val input = readInt()
      if ((input >= 0) && (input <= 2)) {
        return input
      }
      Console.println("Wrong input, please try again")
    }
    -1
  }

  def getPlayerAction = {
    (promptForCoordinate("Select the x coordinate for your action [between 0 & 2]"),
      promptForCoordinate("Select the y coordinate for your action [between 0 & 2]"))
  }

  def gamePlayer() = {
    new AlphaBetaSearch[TicTacToeState](TicTacToe)
  }


  def playGame() {
    var currentState = createTicTacToeState(List(0, 0, 0, 0, 0, 0, 0, 0, 0))
    val gamePlayer = new AlphaBetaSearch[TicTacToeState](TicTacToe)

    while (currentState.getUtility() == 0) {
      currentState.turn() match {
        case 1 =>
          val nextMove: TicTacToeAction = gamePlayer.decision(currentState).asInstanceOf[TicTacToeAction]
          currentState = currentState.stateWithAction(1, nextMove.position)
          currentState.draw()
        case -1 =>
          currentState = currentState.stateWithAction(-1, getPlayerAction)
          currentState.draw()
      }
    }
  }

  def main(args: Array[String]) {
    playGame


  }
}
