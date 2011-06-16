package com.file
import java.io._

object Control {

	def using[A<:{ def close():Unit },B] (param:A) (f:A=>B):B = 
		try {
			f(param);
		}
		finally {
			if (param!=null)
				param.close();
		} 

	def reader(is:InputStream):Option[BufferedReader] = {
		is match {
			case null => None
			case _ => try {
						Some(new BufferedReader(new InputStreamReader(is)))
						}
						catch {
							case _ => None 
						}
		}
	}

	def toString(is:InputStream):String = {
		readLines(is).foldLeft("")(_ + _)	
	}

	
	def readLines(is:InputStream):List[String] = {
		reader(is) match {
			case None => Nil
			case Some(br:BufferedReader) => readLines(br)
			case _ => Nil
		}
	}
	
	def readLines(br:BufferedReader):List[String] = {
		var ret:List[String] = Nil		
		def readAll():Unit = br.readLine match {
			case null => 
			case line => ret ::= line; readAll();
		}		
		readAll();		
		ret.reverse
	}
	
}