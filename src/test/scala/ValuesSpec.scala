
package edu.ucsb.pllab.pie

import org.scalatest._
import scala.util.parsing.combinator._
import scala.util._

class ValueLevelParserSpec extends FlatSpec with Matchers {

	import values._

	object Parser extends RegexParsers 
		with ValueParser 
		with ParserHelper[Value] {
		
		override def main = application
	}
	
	"Function application" should "be left-associative" in {
		assert(Parser("a b c") == Success(App(App(Id('a), Id('b)), Id('c))))
		assert(Parser("a (b c)") == Success(App(Id('a), App(Id('b), Id('c)))))
	}

	// write more tests here

}