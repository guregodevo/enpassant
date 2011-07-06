package com.chess.parse

import scala.util.parsing.combinator._

object Termination extends Enumeration {
  type Termination = Value
  val white, black, draw = Value
}

import Termination._

case class Game(tags: List[Tag], moves:List[MoveElement], termination: Termination)
case class Tag(name: String, value: String)
case class MoveElement(move: SAN, comment: String)
case class SAN(value: String)

object PGNParser extends JavaTokenParsers with ImplicitConversions with RunParser {

  type RootType = List[Game]

  def moveNumberParser: Parser[Int] = ("""\d+""".r <~ """\.+""".r) ^^ (_.toInt)

  def tagSectionToken: Parser[List[Tag]] =
    (tagPairParser +) ^^ (List() ++ _)

  def tagPairParser: Parser[Tag] = {
    def toTag(name: String, value: String): Tag = Tag(name, value)
    "[" ~> ident ~ stringLiteral <~ "]" ^^ toTag
  }

  def gameTerminationParser: Parser[Termination] = {
    def toGameTermination (t: String): Termination = {
      t match {
        case "0-1" => black
        case "1-0" => white
        case "1/2-1/2" => draw
      }
    }
    ("0-1" | "1-0" | "1/2-1/2") ^^ toGameTermination
  }

  def moveTextSectionParser: Parser[List[MoveElement]] =
    (elementParser +) ^^ (List() ++ _)

  def elementParser: Parser[MoveElement] = {
    def toMoveElement(move: SAN, comment: Option[String]): MoveElement =
      MoveElement(move, comment.getOrElse(null))
    (moveNumberParser ?) ~> sanMoveParser ~ (commentParser ?) ^^ toMoveElement
  }

  def commentParser: Parser[String] = "\\{.*\\}".r

  def sanMoveParser: Parser[SAN] = {
    ((pawnMoveWithPromotionParser |
      pawnCaptureWithPromotionParser |
      pawnMoveParser |
      pawnCaptureParser |
      pieceMoveParser |
      pieceCaptureParser |
      castlingParser ) <~ """[+#]?""".r) ^^ (SAN(_))
  }

  def pawnMoveParser: Parser[String] = """[a-h][1-8]""".r
  def pawnCaptureParser: Parser[String] = """[a-h]x[a-h][1-8]""".r
  def pieceMoveParser: Parser[String] = """[KQBNR][a-h]?[1-8]?[a-h][1-8]""".r
  def pieceCaptureParser: Parser[String] = """[KQBNR][a-h]?[1-8]?x[a-h][1-8]""".r
  def pawnMoveWithPromotionParser: Parser[String] = """[a-h][18]=[QBNR]""".r
  def pawnCaptureWithPromotionParser: Parser[String] = """[a-h]x[a-h][18]=[QBNR]""".r
  def castlingParser: Parser[String] = """O-O-O""" | """O-O"""

  def gameParser: Parser[Game] = {
    def toGame(tags: List[Tag], moves: List[MoveElement], termination: Termination) = Game(tags, moves, termination)
    (tagSectionToken ~ moveTextSectionParser ~ gameTerminationParser) ^^ toGame
  }
  
  def root:Parser[List[Game]] = (gameParser +) ^^ (List() ++ _)




}