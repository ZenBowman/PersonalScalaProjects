package org.zenbowman.adversarial

abstract class Game[T <: GameState] {
  def getSuccessors(state: T): List[T]

  def isTerminal(state: T): Boolean

  def actionForSuccessor(currentState: T, successor: T): Option[Action]

  def draw(state: T)
}

trait Action {
}

trait GameState {
  def getUtility(): Int
}