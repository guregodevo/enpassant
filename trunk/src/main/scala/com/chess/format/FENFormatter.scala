package com.chess.format

import com.chess._

object FenFormatter extends SANFormatter {
    
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
        val sortedSquares = sort(arg0.cases)
      	val fen = for { i<-(0 to 7).reverse } yield toFEN(sortedSquares(i),0)
      	fen.elements.mkString("/") + " " + List(arg0.c,castlingFEN,enpassantFEN,arg0.halfMv,arg0.fullMv).mkString(" ")
   }
  
  
  def sort(cases:List[List[Piece]]):List[List[Piece]] = {         
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
