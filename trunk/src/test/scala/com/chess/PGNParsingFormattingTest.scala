package com.chess

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import com.chess._
import com.chess.parse._
import com.chess.format._
import scala.util.parsing.combinator._
import java.io._

@RunWith(classOf[JUnitRunner])
class PGNParsingFormattingTest extends Spec with ShouldMatchers {
  
  describe("PGN notation parsing") {
    it("should parse PGN file for standard game PGN notation") {
      val inputStream = this.getClass().getResourceAsStream("the_world_kasparov_1999.pgn")
      val reader:Reader = new BufferedReader(new InputStreamReader(inputStream))
      val b = PGNParser.parse(reader)
      println(b)
    }
    it("should parse PGN file for my game PGN file") {
      val inputStream = this.getClass().getResourceAsStream("me_vs_Computer.pgn")
      val reader:Reader = new BufferedReader(new InputStreamReader(inputStream))
      val b = PGNParser.parse(reader)
      println(b)
    }

  }
  
}

