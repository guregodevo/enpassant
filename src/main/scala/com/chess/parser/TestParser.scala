package com.chess.parse

import scala.util.parsing.combinator._

class TestParser {

}


object Test01 extends FenParser {

  def colorP: Parser[Char] = elem(' ') ~> ( elem('w') | elem('b') ) 

  def castlingP: Parser[List[Char]] = elem(' ') ~> rep( elem('K') ^^^ 'K' | elem('Q') ^^^ 'Q' | elem('q') ^^^ 'q' | elem('k') ^^^ 'k' | elem('-')  ^^^ '-' ) 

  
  val psr = phrase(
        ("an" ~ "other" ~ "day") ^^ { case x ~ y ~ z => y} // Matches ONLY "an other day" NOTHING else
      | "anotherday"  ^^ {Pair(6, _)}) // Matches ONLY "anotherday" NOTHING else
    
  def main(args: Array[String]) {
    println(parseAll(castlingP, " -"))
    println(parseAll(castlingP, " KQqk"))
    println(parseAll(moveParser, "a1"))
    println(parseAll(moveParser, "-"))
    println(parseAll(enPassantParser, " -"))
  }
}