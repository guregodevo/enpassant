package com.chess

import scala.collection.immutable._;
import java.lang.IllegalArgumentException;
import scala.math._;

 class Position(x: Int, y:Int) {
   
   def --(f:Pair[Int,Int]=>Pair[Int,Int]):Pair[Int,Int] = {
	   f((x,y))
   }
   
 }
 
class Board(cases:List[List[Piece]],c:Char,castlingK:Boolean,castlingQ:Boolean, castlingk:Boolean,castlingq:Boolean,enPassant:Option[Pair[Int,Int]],halfMv:Int,fullMv:Int) {
 
	implicit def tupleConvertor(x: Tuple2[Int,Int]): Position = {
	new Position(x._1,x._2) 
}

  def file(c:Char):Option[Int] = {
	  c match {
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
  
  def file(i:Int):Option[Char] = {
	  i match {
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

  def algebraic(t:Pair[Int,Int]) = {
		file(t._1).getOrElse("") + (t._2+1).toString   
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
  
  def castlingFEN():String =
  {
	  if (!this.castlingK && !this.castlingQ && !this.castlingk && !this.castlingq)
	 	  "-"
	  else
	 	  (if (this.castlingK) "K" else "") + (if (this.castlingQ) "Q" else "") + (if (this.castlingk) "k" else "") + (if (this.castlingq) "q" else "")
  }
  
  def enpassantFEN():String =
  {
	  this.enPassant match {
	 	  case None => "-"
	 	  case Some((x,y)) => algebraic((x,y))  
	  }
  }  
  
  def toFEN():String = {
	  val turn = -|(cases)
	  val fen = for { i<-(0 to 7).reverse } yield toFEN(turn(i),0)
	  
	  fen.elements.mkString("/") + " " + List(this.c,castlingFEN,enpassantFEN,this.halfMv,this.fullMv).mkString(" ")
  }

  def toFEN(row:List[Piece],countO:Int):String = {
	  row match {
	 	  case Nil => (if (countO>0) countO.toString else "")
	 	  case O::rest => toFEN(rest,countO+1)
	 	  case s::rest => (if (countO>0) countO.toString else "")+s.toString+toFEN(rest,0)
	  }	  
  }

  def printit() = {
	  for { i<-(0 to 7).reverse	
	 	    j<-(0 to 7)}
	   {
	   print("|"+cases(j)(i).toString)
	   if (j==7) println ("|")
	   }
  }
	  
  def <->(from:Pair[Int,Int],to:Pair[Int,Int]):Board = {
	  val p = piece(from._1,from._2)
	  def rep(i:Int, file:List[Piece]):List[Piece] = {
	 	 val c = if (i == from._1) cases(from._1).updated(from._2,O) else file
	 	 if (i == to._1) c.updated(to._2,cases(from._1)(from._2)) else c
	  }
	  def repFiles(i:Int, l:List[List[Piece]]):List[List[Piece]] = {
	 	  l match {
	 	 	  case x::xs => rep(i,x)::repFiles(i+1,xs)
	 	 	  case Nil => l
	 	  }
	  }
	  var enPassantPawn:Option[Pair[Int,Int]] = None;
	  if (this.c=='w' && from._2==1 && to._2==3)
	    enPassantPawn=Some(to)
	  else if (this.c=='b' && from._2==6 && to._2==4)
	    enPassantPawn=Some(to)
	    
	  val color = (if (this.c == 'w') 'b' else 'w' )
	  //p match {
	 //	  case K('w') => this.castlingK;
	 	//  case K('b') =>
	  //}
	  //TODO roque / grand roque
	  //en passant
	  
	  Board(repFiles(0,cases),color,this.castlingK, this.castlingQ, this.castlingk, this.castlingq, enPassantPawn, this.halfMv+1, this.fullMv+1)
  }

  def ---(p:Pair[Int,Int],f: Pair[Int,Int] => Pair[Int,Int]):List[Pair[Int,Int]] = {
	  val next = f(p._1,p._2)
	  piece(next) match {
	 	  case Some(O) => next:: ---(next,f) 
	 	  case Some(d:Piece) if d.sameColor(piece(p)) => Nil
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

  def checkmate(c:Char):Boolean = {
	  val t = where(K(c)).get
	  royal( t ).forall(x => check(x,c) )
  }

  def check(c:Char):Boolean = {
		  val t = where(K(c)).get
		  check(t,c)
  }
  
  def check(t:(Int,Int),c:Char):Boolean = {
		  val p = K(c)
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
			case Some(r:K) => Nil			   
			case Some(O) => Nil			   
			case _ => Nil
	  }
  }  

  def legal(from:Pair[Int,Int],to:Pair[Int,Int]):Boolean =
  {
	  legalMoves(from).contains(to)
  }
  
}

object Board {
	
	lazy val _b = {
		 val ah = R('w')::P('w')::O::O::O::O::P('b')::R('b')::Nil
		 val bg = N('w')::P('w')::O::O::O::O::P('b')::N('b')::Nil
		 val cf = B('w')::P('w')::O::O::O::O::P('b')::B('b')::Nil
		 val d  = Q('w')::P('w')::O::O::O::O::P('b')::Q('b')::Nil
		 val e  = K('w')::P('w')::O::O::O::O::P('b')::K('b')::Nil
		 val l:List[List[Piece]] = List(ah,bg,cf,d,e,cf,bg,ah)
		 val y = new Board(l,'b', true, true, true, true ,None,0,1)
		 y
	}

	def apply():Board = {
		 _b
	}
		
	def apply(l:List[List[Piece]],c:Char,castlingK:Boolean,castlingQ:Boolean, castlingk:Boolean,castlingq:Boolean,enPassant:Option[Pair[Int,Int]],halfMv:Int,fullMv:Int):Board = {
		 val b = new Board(l,c,castlingK,castlingQ, castlingk,castlingq,enPassant,halfMv,fullMv)
		 b
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

case class R(color:Char) extends Piece
case class B(color:Char) extends Piece 
case class K(color:Char) extends Piece
case object O extends Piece {
	def color():Char = 'w'
}
case class N(color:Char) extends Piece
case class Q(color:Char) extends Piece
case class P(color:Char) extends Piece

