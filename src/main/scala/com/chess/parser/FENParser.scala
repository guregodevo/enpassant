package com.chess.parse

import com.chess._
import scala.util.parsing.combinator._

trait AlgebraicParser extends RegexParsers {

  lazy val file:Parser[Int] = 	  
	 	  (elem('a') | elem('A')) ^^^ 0 |
	 	  (elem('b') | elem('B')) ^^^ 1 |
	 	  (elem('c') | elem('C')) ^^^ 2 |
	 	  (elem('d') | elem('D')) ^^^ 3 |
	 	  (elem('e') | elem('E')) ^^^ 4 |
	 	  (elem('f') | elem('F')) ^^^ 5 |
	 	  (elem('g') | elem('G')) ^^^ 6 |
	 	  (elem('h') | elem('H')) ^^^ 7 

  lazy val rank:Parser[Int] = elem("Rank digit", c => c >='1' && c <='9') ^^ (_.toInt - 49 )

  lazy val moveParser:Parser[Option[(Int,Int)]] = 
    elem('-') ^^^ None | 
    file ~ rank ^^ { case f ~ r => Some((f,r)) }


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
  
  def toChar(file:Int):Option[Char] = {
	file match {
		  case 0 => Some('a')
		  case 1 => Some('b')
		  case 2 => Some('c')
		  case 3 => Some('d')
		  case 4 => Some('e')
		  case 5 => Some('f')
		  case 6 => Some('g')
		  case 7 => Some('f')
		  case _ => None
	}
  }

  def toString(t:Pair[Int,Int]) = {
		toChar(t._1).getOrElse("") + (t._2+1).toString   
  }
  
}

object FenConvertor extends FenParser {
  
  def parse(fen:String):ChessBoard = { 
		println(run(fen).get)
       	run(fen).get // .getOrElse(ChessBoard()) 
  }
  
  def format(arg0:ChessBoard):String = {
      	val castlingFEN =
      	{
      			if (!arg0.castlingK && !arg0.castlingQ && !arg0.castlingk && !arg0.castlingq)
      				"-"
      			else
      				(if (arg0.castlingK) "K" else "") + (if (arg0.castlingQ) "Q" else "") + (if (arg0.castlingk) "k" else "") + (if (arg0.castlingq) "q" else "")
      	}
      	val enpassantFEN = arg0.enPassant match {
	 	  case None => "-"
	 	  case Some((x,y)) => super.toString((x,y))  
      	}
        val turn = -|(arg0.cases)
      	val fen = for { i<-(0 to 7).reverse } yield toFEN(turn(i),0)
      	fen.elements.mkString("/") + " " + List(arg0.c,castlingFEN,enpassantFEN,arg0.halfMv,arg0.fullMv).mkString(" ")
   }
  
  

  
  def -|(cases:List[List[Piece]]):List[List[Piece]] = {         
                  for { i <- List.range(0,8) } yield List.range(0,8).map( j => cases(j)(i))               
  }          
 
  def toFEN(squares:List[Piece],countO:Int):String = {
	  squares match {
	 	  case Nil => (if (countO>0) countO.toString else "")
	 	  case O::rest => toFEN(rest,countO+1)
	 	  case s::rest => (if (countO>0) countO.toString else "")+s.toString+toFEN(rest,0)
	  }	  
  }
  
}

trait RunParser {
	this : RegexParsers =>
	
	type RootType
	
	def root:Parser[RootType]
	
	def run(in:String):ParseResult[RootType] = parseAll(root,in)
	
}

trait FenParser extends AlgebraicParser with RunParser {

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
	
	lazy val piece = elem('R')|elem('B')|elem('N')|elem('K')|elem('Q')|elem('P')|elem('B')|elem('r')|elem('n')|elem('k')|elem('q')|elem('p')|elem('b')

	def pieceP = piece ^^ {
		case c:Char => toPiece(c)::Nil
		case _ => Nil	 	  
	}

	lazy val decimalDigit = elem("Decimal Digit", c => c >='0' && c<='9')	

	lazy val rankSquares = rep(pieceP|empty)

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

	lazy val castlingParser: Parser[List[Char]] = elem(' ') ~> rep( elem('K') | elem('Q') | elem('q') | elem('k') | elem('-')  ) 

	lazy val enPassantParser: Parser[Option[(Int,Int)]] = elem(' ') ~> moveParser

	lazy val number = "[0-9]+[ \\n]+".r

	lazy val numberParser: Parser[Int] = elem(' ') ~> """\d+""".r ^^ (_.toInt)

	lazy val colorParser: Parser[Char] = elem(' ') ~> ( elem('w') | elem('b') )

	lazy val squares = repsep( rankSquares , "/" )

	lazy val gameParser: Parser[ChessBoard] = {
			def toChessboard(l:List[List[List[Piece]]],c:Char,castlingK:Boolean,castlingQ:Boolean,castlingk:Boolean,castlingq:Boolean, enPassant:Option[(Int,Int)],halfMvClock:Int,fullMv:Int) = {
			  ChessBoard(reorder(l),c,castlingK,castlingQ,castlingk,castlingq,enPassant,halfMvClock,fullMv)
			}
			(squares ~ colorParser ~ castlingParser ~ enPassantParser ~ numberParser ~ numberParser) ^^ { case s ~ c ~ castling ~ enPassant ~ halfMvClock ~ fullMv => toChessboard(s,c,castling.contains('K'),castling.contains('Q'),castling.contains('k'),castling.contains('q'),enPassant,halfMvClock, fullMv) }
	}

	def reorder(res:List[List[List[Piece]]]) = {
		 def order(cases:List[List[Piece]]):List[List[Piece]] = { 	
			 for { i <- List.range(0,8) } 
			 	yield List.range(0,8).reverse.map( j => cases(j)(i))		  
		 }
		 order(res.map(x => x.flatten))
	}
	
	def root = gameParser


}