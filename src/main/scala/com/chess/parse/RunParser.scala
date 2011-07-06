package com.chess.parse

import java.io.Reader
import scala.util.parsing.combinator._

trait RunParser {
	this : RegexParsers =>
	
	type RootType
	
	def root:Parser[RootType]

	def parse(input: String) : ParseResult[RootType] =	parseAll (root, input) 

	def parse(input: Reader) : ParseResult[RootType] = parseAll (root, input) 

}
