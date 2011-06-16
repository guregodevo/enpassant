package com.chess.pgn

import com.chess._
import scala.util.parsing.combinator._

trait RunParser {
	this : RegexParsers =>
	
	type RootType
	
	def root:Parser[RootType]
	
	def run(in:String):ParseResult[RootType] = parseAll(root,in)
	
}

object PieceParser extends JavaTokenParsers with RunParser {
	type RootType = Any

	  def empty = decimalDigit ^^ {
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

	  def piece = elem('R')|elem('B')|elem('N')|elem('K')|elem('Q')|elem('P')|elem('B')|elem('r')|elem('n')|elem('k')|elem('q')|elem('p')|elem('b')

	  def pieceP = piece ^^ {
	  	case c:Char => toPiece(c)::Nil
	  	case _ => Nil	 	  
	  }
	   
	  
	  def decimalDigit = elem("Decimal Digit", c => c >='0' && c<='9')	
	  	   
	  lazy val rank = rep(pieceP|empty)
	  	  

		def toPiece(c:Char):Piece = {
			c match {
				case 'R' => R('w')
				case 'r' => R('b')
				case 'B' => B('w')
				case 'b' => B('b')
				case 'N' => N('w')
				case 'n' => N('b')
				case 'K' => K('w')
				case 'k' => K('b')
				case 'Q' => Q('w')
				case 'q' => Q('b')
				case 'P' => P('w')		
				case 'p' => P('b')
			}
		}	

		def root = repsep(rank,"/")
  

}