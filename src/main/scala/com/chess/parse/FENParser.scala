package com.chess.parse

import com.chess._
import scala.util.parsing.combinator._

trait SANParser extends RegexParsers {

  lazy val fileChar:Parser[Int] = 	  
	 	  (elem('a') | elem('A')) ^^^ 0 |
	 	  (elem('b') | elem('B')) ^^^ 1 |
	 	  (elem('c') | elem('C')) ^^^ 2 |
	 	  (elem('d') | elem('D')) ^^^ 3 |
	 	  (elem('e') | elem('E')) ^^^ 4 |
	 	  (elem('f') | elem('F')) ^^^ 5 |
	 	  (elem('g') | elem('G')) ^^^ 6 |
	 	  (elem('h') | elem('H')) ^^^ 7 

  lazy val rankDigit:Parser[Int] = elem("Rank digit", c => c >='1' && c <='9') ^^ (_.toInt - 49 )

  lazy val move:Parser[Option[(Int,Int)]] = 
    elem('-') ^^^ None | 
    fileChar ~ rankDigit ^^ { case f ~ r => Some((f,r)) }


  def toInt(file:Char):Option[Int] = {
	  file match {
	 	  case 'a' | 'A' => Some(0)
	 	  case 'b' | 'B' => Some(1)
	 	  case 'c' | 'C' => Some(2)
	 	  case 'd' | 'D' => Some(3)
	 	  case 'e' | 'e' => Some(4)
	 	  case 'f' | 'F' => Some(5)
	 	  case 'g' | 'G' => Some(6)
	 	  case 'h' | 'H'  => Some(7)
	 	  case _ => None
	  }
  }
  
}

trait RunParser {
	this : RegexParsers =>
	
	type RootType
	
	def root:Parser[RootType]

	def parse(input: String) : ParseResult[RootType] =	parseAll (root, input) 

}

object FENParser extends SANParser with RunParser {

	type RootType = ChessBoard
	
	lazy val empty = decimalDigit ^^ {
		case '1' => O::Nil
		case '2' => O::O::Nil
		case '3' => O::O::O::Nil
		case '4' => O::O::O::O::Nil
		case '5' => O::O::O::O::O::Nil
		case '6' => O::O::O::O::O::O::Nil
		case '7' => O::O::O::O::O::O::O::Nil
		case '8' => O::O::O::O::O::O::O::O::Nil
		case '9' => O::O::O::O::O::O::O::O::O::Nil
		case _ => O::Nil
	}
	
	lazy val piece = ( elem('R')| elem('B')|elem('N')|elem('K')|elem('Q')|elem('P')|elem('B')|elem('r')|elem('n')|elem('k')|elem('q')|elem('p')|elem('b')) ^^ {
		case c:Char => Piece.toPiece(c)::Nil
		case _ => Nil	 	  
	}

	lazy val decimalDigit = elem("Decimal Digit", c => c >='0' && c<='8')	

	lazy val rankPlacement = rep(piece|empty)

	lazy val castlingState: Parser[List[Char]] = ' ' ~> rep( elem('K') | elem('Q') | elem('q') | elem('k') | elem('-') ) 

	lazy val enPassant: Parser[Option[(Int,Int)]] = ' ' ~> move

	lazy val number: Parser[Int] = ' ' ~> """\d+""".r ^^ (_.toInt)

	lazy val activeColor: Parser[Char] = ' ' ~> ( elem('w') | elem('b') )

	lazy val piecePlacement = repsep( rankPlacement , '/' )

	lazy val gameParser: Parser[ChessBoard] = {
		def toChessboard(l:List[List[List[Piece]]],c:Char,castlingK:Boolean,castlingQ:Boolean,castlingk:Boolean,castlingq:Boolean, enPassant:Option[(Int,Int)],halfMvClock:Int,fullMv:Int) = {
		  ChessBoard(sort(l),c,castlingK,castlingQ,castlingk,castlingq,enPassant,halfMvClock,fullMv)
		}
		(piecePlacement ~ activeColor ~ castlingState ~ enPassant ~ number ~ number) ^^ { case s ~ c ~ castling ~ enPassant ~ halfMvClock ~ fullMv => toChessboard(s,c,castling.contains('K'),castling.contains('Q'),castling.contains('k'),castling.contains('q'),enPassant,halfMvClock, fullMv) }
	}

	def sort(res:List[List[List[Piece]]]) = {
		 def order(cases:List[List[Piece]]):List[List[Piece]] = { 	
			 for { i <- List.range(0,8) } 
			 	yield List.range(0,8).reverse.map( j => cases(j)(i))		  
		 }
		 order(res.map(x => x.flatten))
	}
	
	lazy val root = gameParser

}