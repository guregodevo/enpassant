package com.chess.format

trait SANFormatter {

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
