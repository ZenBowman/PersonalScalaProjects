package org.zenbowman.adversarial.kalah

import org.zenbowman.adversarial.AlphaBetaSearch

object Kalah {
  val kalahPlayer = new AlphaBetaSearch[KalahState](KalahGame)
  val player1Positions = 0 until 6
  val player1SeedStore = 6
  val player2Positions = 7 until 13
  val player2SeedStore = 13

  def play(state: KalahState) = {
    kalahPlayer.decision(state).asInstanceOf[KalahAction]
  }

  def moveSeed(from: Int): Int = {
    (from + 1) % 14
  }

  def storeForPlayer(player: Int) = {
    player match {
      case 1 => player1SeedStore
      case _ => player2SeedStore
    }
  }

  def movablePositions(kalahState: KalahState) = {
    kalahState.whoseTurn match {
      case 1 => player1Positions
      case _ => player2Positions
    }
  }

  def validMoves(kalahState: KalahState) = {
    val movables = movablePositions(kalahState)
    val result = for {
      m <- movables
      if kalahState.positions(m) > 0
    }
    yield m
    result
  }

  def ceiling(value: Int)(max: Int) = {
    if (value < max) {
      value
    } else {
      max
    }
  }

  def mapSeedDestination(startPosition: Int, numHops: Int) = {
    if (startPosition < 6) {
      ceiling(startPosition + numHops)(6)
    } else {
      ceiling(startPosition + numHops)(13)
    }
  }

  def delta(oldPositions: List[Int], newPositions: List[Int], position: Int) = {
    newPositions(position) - oldPositions(position)
  }

  def switchTurn(turn: Int) = if (turn == 1) 2 else 1

  def captureDestinationFor(house: Int): Option[Int] = {
    if ((house == 6) || (house == 13)) {
      None
    } else {
      Some(12 - house)
    }
  }

  def moveSeeds(kalahState: KalahState, move: Int) = {
    val currentPositions = kalahState.positions.toBuffer
    val numSeeds = currentPositions(move)
    for (i <- 0 until numSeeds) {
      val seedDestination = mapSeedDestination(move, i + 1)
      currentPositions(move) = currentPositions(move) - 1
      currentPositions(seedDestination) = currentPositions(seedDestination) + 1
      if (currentPositions(seedDestination) == 1) {
        for (cp <- captureDestinationFor(seedDestination)) {
          currentPositions(seedDestination) = currentPositions(seedDestination) + currentPositions(cp)
          currentPositions(cp) = 0
        }
      }
    }

    val newPositions = currentPositions.toList
    if (delta(kalahState.positions, newPositions, storeForPlayer(kalahState.whoseTurn)) > 0) {
      new KalahState(newPositions, kalahState.whoseTurn)
    } else {
      new KalahState(newPositions, switchTurn(kalahState.whoseTurn))
    }
  }

  def nextStateForMove(kalahState: KalahState, move: Int) = {
    if (!validMoves(kalahState).contains(move)) {
      throw new Exception("Invalid move: %s for state [%s]".format(move, kalahState))
    }
    moveSeeds(kalahState, move)
  }

  def initialState = {
    new KalahState(List(3, 3, 3, 3, 3, 3, 0, 3, 3, 3, 3, 3, 3, 0), 1)
  }
}
