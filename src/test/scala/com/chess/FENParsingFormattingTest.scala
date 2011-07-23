package com.chess

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import com.chess._
import com.chess.parse._
import com.chess.format._
import scala.util.parsing.combinator._

@RunWith(classOf[JUnitRunner])
class FENParsingFormattingTest extends Spec with ShouldMatchers {
  describe("FEN notation parsing") {
    it("should parse FEN for the starting position") {
       	val fenIn = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
       	val b = FENParser.parse(fenIn).get
       	val fenOut = FenFormatter.format(b)
    	(fenIn) should be (fenOut) 
    }
    it("should parse FEN after the move 1. e4") {
    	val fenIn = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
       	val b = FENParser.parse(fenIn).get
       	val fenOut = FenFormatter.format(b)
    	(fenIn) should be (fenOut) 
    }
    it("should parse FEN after 1. ... c5") {
    	val fenIn = "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"
       	val b = FENParser.parse(fenIn).get
       	val fenOut = FenFormatter.format(b)
    	(fenIn) should be (fenOut) 
    }
    it("should parse FEN after the move 2. Nf3") {
    	val fenIn = "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2"
       	val b = FENParser.parse(fenIn).get
       	val fenOut = FenFormatter.format(b)
    	(fenIn) should be (fenOut) 
    }
    
  }

  describe("FEN picked up from chess website") {
    it("should parse FEN of Lasker endgame") {
    	val fenIn = "6q1/6p1/2k4p/R6B/p7/8/2P3P1/2K5 w - - 0 1"
       	val b = FENParser.parse(fenIn).get
       	val fenOut = FenFormatter.format(b)
    	(fenIn) should be (fenOut) 
    }
  }

}

