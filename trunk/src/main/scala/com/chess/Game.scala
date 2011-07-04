package com.chess

import scala.collection.immutable._;
import java.lang.IllegalArgumentException;
import scala.math._;

 class Position(x: Int, y:Int) {
   
   def --(f:Pair[Int,Int]=>Pair[Int,Int]):Pair[Int,Int] = {
	   f((x,y))
   }
   
 } 
 
 
 class ChessBoard(val cases:List[List[Piece]],val c:Char,val castlingK:Boolean,val castlingQ:Boolean, val castlingk:Boolean, val castlingq:Boolean, val enPassant:Option[Pair[Int,Int]], val halfMv:Int, val fullMv:Int) {
	lazy val KING_ORIGINAL_POSITION = Map( 'b' -> (4,7), 'w' -> (4,0) )
	lazy val ROOK_QUEENSIDE_POSITION = Map( 'b' -> (0,7), 'w' -> (0,0) )
	lazy val ROOK_KINGSIDE_POSITION = Map( 'b' -> (7,7), 'w' -> (7,0) )
	
	implicit def tupleConvertor(x: Tuple2[Int,Int]): Position = {
			new Position(x._1,x._2) 
	}
  
  def piece(t:Pair[Int,Int]):Option[Piece] = {
	  piece(t._1,t._2)
  }

  def piece(rank:Int,file:Int):Option[Piece] = {
	  try 
	    Some(cases(rank)(file)) 
	   catch {
	 	  case _ => None
	  }
  }

  def isBlank(i:Int,j:Int):Boolean = {
	  piece(i,j) match {
	 	  case Some(O) => true
	 	  case _ => false
	  }
  }

  def -|(cases:List[List[Piece]]):List[List[Piece]] = { 	
		  for { i <- List.range(0,8) } yield List.range(0,8).map( j => cases(j)(i))		  
  }

  def prettyPrint() = {
	  for { i<-(0 to 7).reverse	
	 	    j<-(0 to 7)}
	   {
	   print("|"+cases(j)(i).toString)
	   if (j==7) println ("|")
	   }
  }

  def castleKingside(p:Piece, from:Pair[Int,Int]):List[Pair[Int,Int]] = {    
    val isPermissible = this.c match {
      case 'w' => this.castlingK && canCastle(from, ---(from, >>()))
      case 'b' => this.castlingk && canCastle(from, ---(from, >>()))
      case _ => false
    }
    if (isPermissible) 
      %%(>>(),>>())(from)::Nil 
    else
      Nil
  }
  
  def castleQueenside(p:Piece, from:Pair[Int,Int]):List[Pair[Int,Int]] = {    
    val isPermissible = this.c match {
      case 'w' => this.castlingQ && canCastle(from, ---(from, <<()))
      case 'b' => this.castlingq && canCastle(from, ---(from, <<()))
      case _ => false
    }
    if (isPermissible) 
      %%(<<(),<<())(from)::Nil 
    else
      Nil
  }


  def canCastleQueenSide(p:Option[Piece],from:Pair[Int,Int]) = {
    p match {
      case Some(R(color)) => ROOK_QUEENSIDE_POSITION(color) == from
      case Some(K(color)) => KING_ORIGINAL_POSITION(color) == from 
      case _ => true
    }  
  }
  
  def canCastleKingSide(p:Option[Piece],from:Pair[Int,Int]) = {
    p match {
      case Some(R(color)) => ROOK_KINGSIDE_POSITION(color) == from
      case Some(K(color)) => KING_ORIGINAL_POSITION(color) == from 
      case _ => true
    }  
  }
    
  def canCastle(from:Pair[Int,Int], castlingMoves : List[(Int,Int)]):Boolean = {
    val noCheck = !this.check(this.c)
    lazy val noPiecesBetween = castlingMoves.length == 2
    lazy val dontPassThruSquareUnderAttack = castlingMoves.forall( x => !this.check(x,this.c))  
    noCheck && noPiecesBetween && dontPassThruSquareUnderAttack
  }

  def <>(squares:List[List[Piece]], from:Pair[Int,Int],to:Pair[Int,Int]):List[List[Piece]] = {
    def rep(i:Int, file:List[Piece]):List[Piece] = {
	 	 val fromFile = if (i == from._1) squares(i).updated(from._2,O) else file
	 	 if (i == to._1) fromFile.updated(to._2,squares(from._1)(from._2)) else fromFile
	}
	
    def repFiles(i:Int, files:List[List[Piece]]):List[List[Piece]] = {
	 	  files match {
	 	 	  case x::xs => rep(i,x)::repFiles(i+1,xs)
	 	 	  case Nil => files
	 	  }
	}
    repFiles(0,squares)
  }
  
  def <->(from:Pair[Int,Int],to:Pair[Int,Int]):ChessBoard = {
	  val p = piece(from._1,from._2)
	  var enPassantPawn:Option[Pair[Int,Int]] = None;
	  if (this.c=='w' && from._2==1 && to._2==3)
	    enPassantPawn=Some(to)
	  else if (this.c=='b' && from._2==6 && to._2==4)
	    enPassantPawn=Some(to)
	  
	  var isCastlingK = (if (this.c == 'w') this.castlingK && canCastleKingSide(p, from) else this.castlingK )
	  var isCastlingk = (if (this.c == 'b') this.castlingk && canCastleKingSide(p, from) else this.castlingk )
	  var isCastlingQ = (if (this.c == 'w') this.castlingQ && canCastleQueenSide(p, from) else this.castlingQ )
	  var isCastlingq = (if (this.c == 'b') this.castlingq && canCastleQueenSide(p, from) else this.castlingq )	    
	  val sqrMved = (to._1 - from._1)	  
	  val adjustedSquares = p match {
	    case Some(K(this.c)) if (sqrMved == 2)=>	      	  
	      		  val rookPos = ROOK_KINGSIDE_POSITION(this.c)
	      		  <>(cases, rookPos,(rookPos._1 - 2, rookPos._2))
	    case Some(K(this.c)) if (sqrMved == -2)=> 	      	  
	      		  val rookPos = ROOK_QUEENSIDE_POSITION(this.c)
	      		  <>(cases, rookPos,(rookPos._1 + 2, rookPos._2))
	    case _ => cases
	  }

	  val color = (if (this.c == 'w') 'b' else 'w' )

	  ChessBoard(<>(adjustedSquares,from,to),color,isCastlingK, isCastlingQ, isCastlingk, isCastlingq, enPassantPawn, this.halfMv+1, this.fullMv+1)
  }

  
  def ---(p:Pair[Int,Int],f: Pair[Int,Int] => Pair[Int,Int]):List[Pair[Int,Int]] = {
	  val next = f(p._1,p._2)
	  piece(next) match {
	 	  case Some(O) => next:: ---(next,f) 
	 	  case Some(d:Piece) if d.color == this.c => Nil
	 	  case None => Nil
	 	  case _ => next:: ---(next,f)
	   }
  }  
  
  def <<():Pair[Int,Int]=>Pair[Int,Int] = {
	  x => (x._1 -1, x._2 )
  }
  

  def >>():Pair[Int,Int]=>Pair[Int,Int] = {   
	   x => (x._1 +1, x._2 )
  }

  def +^():Pair[Int,Int]=>Pair[Int,Int] = {   
	   x => (x._1, x._2+1 )
  }

  def -^():Pair[Int,Int]=>Pair[Int,Int] = {   
	   x => (x._1, x._2-1 )
  }
  
  def %%[A](f: A => A, g: A => A):A=>A = {   
	   x => g ( f(x) ) 
  }

  def %%%[A](f: A => A, g: A => A, h:A => A):A=>A = {   
	   x => h ( g ( f(x) ) ) 
  }

  def straight(p:Pair[Int,Int]):List[Pair[Int,Int]] = {
	  (---(p,>>())) ++ (---(p,<<()))++ (---(p,+^()))++ (---(p,-^()))
  }

  def diagonal(p:Pair[Int,Int]):List[Pair[Int,Int]] = {
	  (---(p,%%(>>(),+^()))) ++ (---(p,%%(>>(),-^())))++(---(p,%%(<<(),+^()))) ++ (---(p,%%(<<(),-^()))) 
  }

  def knighty(p:Pair[Int,Int]):List[Pair[Int,Int]] = {
	  val fs = (%%%(+^(),+^(),>>()))::(%%%(+^(),+^(),<<()))::(%%%(-^(),-^(),<<()))::(%%%(-^(),-^(),>>()))::(%%%(<<(),<<(),+^()))::(%%%(<<(),<<(),-^()))::(%%%(>>(),>>(),+^()))::(%%%(>>(),>>(),-^()))::Nil
	  val f = piece(p)
	  fs.map(f => f(p)).filter( t => piece(t) match {
	 	  case None => false
	 	  case Some(O) => true
	 	  case Some(d:Piece) if d.color==f.getOrElse(O).color => false
	 	  case _ => true
	  } )
  }  
  
  def royal(p:Pair[Int,Int]):List[Pair[Int,Int]] = {
	  val fs = (>>())::(<<())::(-^())::(+^())::(%%(-^(),>>()))::(%%(-^(),<<()))::(%%(+^(),>>()))::(%%(+^(),<<()))::Nil
	  val f = piece(p)
	  fs.map(f => f(p)).filter( t => piece(t) match {
	 	  case None => false
	 	  case Some(O) => true
	 	  case Some(d:Piece) if d.sameColor(f) => false
	 	  case _ => true
	  } )
  }

  def legalPawnTakes(p:Pair[Int,Int]):List[Pair[Int,Int]] = {
    val f = piece(p)
	  val takes = f match {
	    case Some(x:Piece) if (x.color=='w')  => (%%(+^(),>>()))::(%%(+^(),<<()))::Nil
	    case Some(x:Piece) if (x.color=='b')  => (%%(-^(),>>()))::(%%(-^(),<<()))::Nil
	    case _ => Nil
	  }

	  takes.map(t => t(p)).filter( t => piece(t) match {
	 	  case Some(O) => false
	 	  case Some(x:Piece) if (x.color!=f.get.color) => true
	 	  case _ => false
	  } )	  
  }
  
  def pawny(p:Pair[Int,Int]):List[Pair[Int,Int]] = {
	  val f = piece(p)
	  val mvs = f match {
	    case Some(P('w')) if (p._2==1)=> +^()::(%%(+^(),+^()))::Nil
	    case Some(P('w')) if (p._2!=1)=> +^()::Nil
	    case Some(P('b')) if (p._2==6)=> -^()::(%%(-^(),-^()))::Nil
	    case Some(P('b')) if (p._2!=6)=> -^()::Nil
	    case _ => Nil
	  }
	  
	  val legalMvs = mvs.map(m => m(p)).filter( t => piece(t) match {
	 	  case Some(O) => true
	 	  case _ => false
	  } )
	  
	  var enPassantMv:List[Pair[Int,Int]] = Nil
	  if (this.enPassant != None) {
	    val dist = ( this.enPassant.get._1 - p._1 )	    
	    if (dist==1 || dist== -1)
	    {
		    if (this.c=='b' && p._2==3)
		      enPassantMv= (this.enPassant.get._1,p._2 - 1 )::Nil
		      else if (this.c=='w' && p._2==4) 
		        enPassantMv= (this.enPassant.get._1,p._2 + 1 )::Nil
	    }
	  }
	  legalMvs++enPassantMv++legalPawnTakes(p)
  }

  def where(p:Piece):Option[(Int,Int)] = {
	 val coords = for { 
	 	  i <- (0 to 7)
	 	  j <- (0 to 7)	
	 	  }	 	  
	 	 yield (i,j)
	 coords.find(x => piece(x) match {
	 		 case Some(e) if e == p => true
	 		 case _ => false
	 	 })
  }
  
  
  def check(t:Pair[Int,Int],p:Piece):Boolean = {
      legalMoves(t).forall(x => piece(x) match {
    	  case Some(r:K) if r.color!=p.color => true
    	  case _ => false    	  
      })
  }

  def getPiecesCoordOfColor(c:Char):Seq[Pair[Int,Int]] = {
    	 val coords = for { 
	 	  i <- (0 to 7)
	 	  j <- (0 to 7)	
	 	  }	 	  
	 	 yield (i,j)
	 coords.filter(x => piece(x) match {
		     								case Some(O) => false
		     								case Some(p:Piece) if p.color == c => true
		     								case _ => false
		   							}
		   )
	

  }

  def stalemate(t:(Int,Int)):Boolean = {
	  getPiecesCoordOfColor( c ).exists( x => this.legalMoves(x).exists( y => !this.<->(x,y).check(t,c) )  ) 
  }

  def stalemate(c:Char):Boolean = {
	  val t = where(K(c)).get
	  stalemate(t) 
  }
    
  def checkmate(c:Char):Boolean = {
	  val t = where(K(c)).get	  
	  check(t, c) && stalemate(t) 
  }

  def check(c:Char):Boolean = {
		  val t = where(K(c)).get
		  check(t,c)
  }
  
  def check(t:(Int,Int),c:Char):Boolean = {
	      lazy val straightcheck = straight(t).exists(x => piece(x) match {
	     	  case Some(d:Q) if d.color != c => true
	     	  case Some(d:R) if d.color != c => true
	     	  case _ => false
	      })
	      lazy val diagonalcheck = diagonal(t).exists(x => piece(x) match {
	     	  case Some(d:Q) if d.color != c => true
	     	  case Some(d:B) if d.color != c => true
	     	  case _ => false
	      })
	      lazy val knightCheck = knighty(t).exists(x => piece(x) match {
	     	  case Some(d:N) if d.color != c => true
	     	  case _ => false
	      })
	      lazy val reyCheck = royal(t).exists(x => piece(x) match {
	     	  case Some(d:K) if d.color != c => true
	     	  case _ => false
	      })      
	      lazy val pawnCheck = legalPawnTakes(t).exists(x => piece(x) match {
	     	  case Some(d:P) if d.color != c => true
	     	  case _ => false
	      })
	      straightcheck || diagonalcheck || knightCheck || reyCheck || pawnCheck
  }
  
    
  def legalMoves(p:Pair[Int,Int]):List[Pair[Int,Int]] = {
	  piece(p) match {
			case Some(r:R) => straight(p) 
			case Some(b:B) => diagonal(p)
			case Some(r:Q) => straight(p)++diagonal(p)
			case Some(pp:P) => pawny(p)		  
			case Some(r:N) => knighty(p)		   
			case Some(r:K) => royal(p)++castleKingside(r, p)++castleQueenside(r, p)			   
			case Some(O) => Nil			   
			case _ => Nil
	  }
  }  

  def legal(from:Pair[Int,Int],to:Pair[Int,Int]):Boolean =
  {
	  legalMoves(from).contains(to)
  }
  
}

object ChessBoard {
	
	lazy val _b = {
		 val ah = R('w')::P('w')::O::O::O::O::P('b')::R('b')::Nil
		 val bg = N('w')::P('w')::O::O::O::O::P('b')::N('b')::Nil
		 val cf = B('w')::P('w')::O::O::O::O::P('b')::B('b')::Nil
		 val d  = Q('w')::P('w')::O::O::O::O::P('b')::Q('b')::Nil
		 val e  = K('w')::P('w')::O::O::O::O::P('b')::K('b')::Nil
		 val l:List[List[Piece]] = List(ah,bg,cf,d,e,cf,bg,ah)
		 val y = new ChessBoard(l,'w', true, true, true, true ,None,0,1)
		 y
	}

	def apply():ChessBoard = {
		 _b
	}
		
	def apply(l:List[List[Piece]],c:Char,castlingK:Boolean,castlingQ:Boolean, castlingk:Boolean,castlingq:Boolean,enPassant:Option[Pair[Int,Int]],halfMv:Int,fullMv:Int):ChessBoard = {
		 val b = new ChessBoard(l,c,castlingK,castlingQ, castlingk,castlingq,enPassant,halfMv,fullMv)
		 b
	}

	def unapply(b:ChessBoard) = {
		 Some((b.cases,b.c,b.castlingK,b.castlingQ, b.castlingk,b.castlingq,b.enPassant,b.halfMv,b.fullMv))
	}

}

sealed trait Piece {
	def color():Char
	def sameColor(p: Option[Piece]):Boolean ={
		color()==p.getOrElse(O).color
	}
		
	override def toString():String = {
		this match {
			case R('w') => "R"
			case R('b') => "r"
			case B('w') => "B"
			case B('b') => "b"
			case N('w') => "N"
			case N('b') => "n"
			case O => "_"
			case K('w') => "K"
			case K('b') => "k"
			case Q('w') => "Q"
			case Q('b') => "q"
			case P('w') => "P"		
			case P('b') => "p"
		}
	}	
}

object Piece {
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
}

case class R(color:Char) extends Piece
case class B(color:Char) extends Piece 
case class K(color:Char) extends Piece
case object O extends Piece {
	def color():Char = 'w'
}
case class N(color:Char) extends Piece
case class Q(color:Char) extends Piece
case class P(color:Char) extends Piece

