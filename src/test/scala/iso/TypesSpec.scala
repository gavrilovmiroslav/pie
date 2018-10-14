
package edu.ucsb.pllab.pie
import edu.ucsb.pllab.pie._
import org.scalatest._
import scala.util.parsing.combinator._
import scala.util._

import types._
class IsoParserSpec extends FlatSpec with Matchers {

	object CParser extends DebugRegexParsers 
		with πTypeParser 
		with ConcreteParser[CombType] {
		
		override def main = comb
	}

	object VParser extends RegexParsers 
		with values.ValueParser 
		with ConcreteParser[values.Value] {
		
		override def main = value
	}
	
	"The state machine" should "be able to prove that 'a × bool <-> 'a + 'a" in {
		import iso._

		val program: CombType = CParser("swap×; distrib; (unite + unite)").get

		val io = Map(
			VParser("(3, left _)").get → VParser("left 3").get,
			VParser("(4, right _)").get → VParser("right 4").get)

		io.foreach { case (input, output) => 
			assert(VM.eval(program, input) == Yes(output))
		}
	}

	it should "compute `distrib;((id × inc) + id); factor` as an `if` construct" in {
		import iso._

		val program: CombType = CParser("distrib;((id × inc) + id); factor").get

		val io = Map(
			VParser("(left _, 3)").get → VParser("(left _, 4)").get,
			VParser("(right _, 3)").get → VParser("(right _, 3)").get)

		io.foreach { case (input, output) => 
			assert(VM.eval(program, input) == Yes(output))
		}
	}
}