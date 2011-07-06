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
