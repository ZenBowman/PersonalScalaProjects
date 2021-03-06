package org.zenbowman.adversarial

import org.scalatest.FlatSpec
import tictactoe._

class TicTacToeTest extends FlatSpec {

  "Two equal states" should "be recognized as thus" in {
    val tts1 = new TicTacToeState(new Grid3x3(List(0, 1, 0, 0, 0, 0, 0, 0, -1)))
    val tts2 = new TicTacToeState(new Grid3x3(List(0, 1, 0, 0, 0, 0, 0, 0, -1)))
    assert(tts1 == tts2)
  }

  "An vertical line" should "be found in this grid" in {
    val p = new Grid3x3(List(1, 0, 0, 1, 0, 0, 1, 0, 0))
    p.draw()
    val tts = new TicTacToeState(p)
    val vline = tts.findVerticalLines()
    assert(vline.get == 1)
  }

  "A horizontal line" should "be found in this grid" in {
    val p = new Grid3x3(List(0, 0, 0, -1, -1, -1, 0, 0, 0))
    val tts = new TicTacToeState(p)
    assert(tts.findHorizontalLines().get == -1)
  }

  "The correct empty positions" should "be found" in {
    val p = new Grid3x3(List(0, 0, 0, -1, -1, -1, 0, 0, 0))
    val es = p.empties()
    assert(es.length == 6)
    val gs = new TicTacToeState(p)
    assert(gs.getUtility() == -1)
  }

  "Six successors" should "be found" in {
    val p = new Grid3x3(List(0, 0, 0, -1, -1, -1, 0, 0, 0))
    val tts = new TicTacToeState(p)
    val successors = TicTacToe.getSuccessors(tts)
    for (s <- successors) s.draw
    assert(successors.length == 6)
  }

  "The correct utility" should "be recognized" in {
    val tts1 = TicTacToeHelper.createTicTacToeState(List(0, 1, 0, 0, 1, 0, -1, -1, -1))
    println(tts1.getUtility())
    expect(-1)(tts1.getUtility())
  }

  "A threat" should "be blocked" in {
    val p = new Grid3x3(List(1, 0, 0, 0, 1, 0, -1, 0, -1))
    val t = new TicTacToeState(p)

    val m1 = new MiniMax[TicTacToeState](TicTacToe)
    val d1 = m1.decision(t)
    expect((1, 2))((d1.asInstanceOf[TicTacToeAction]).position)


    val m2 = new AlphaBetaSearch[TicTacToeState](TicTacToe)
    val d2 = m2.decision(t)
    expect((1, 2))((d2.asInstanceOf[TicTacToeAction]).position)
    assert(m2.count < m1.count)
    println()
    println("States evaluated with minimax: " + m1.count)
    println("States evaluated with alpha-beta: " + m2.count)
  }

}

