
import scala.util.parsing.combinator._
import edu.ucsb.pllab.pie._

package edu.ucsb.pllab.pie.values {
	case class Id(id: Symbol) extends Value {
		override def toString = s"${id.name}"
	}

	case class Func(v: Value, body: Value) extends Value {
		override def toString = s"λ$v. $body"
	}

	case class App(l: Value, r: Value) extends Value {
		override def toString = s"$l $r"
	}

	object λValueLevel {
		def func(v: Value, b: Value) = Func(v, b)
	}


	trait λValueParser extends ValueParser { this: RegexParsers =>

		def funcValue: Parser[Func] = 
			"λ" ~> value ~ "." ~ application ^^ { case v ~ _ ~ b => Func(v, b) }

		def identifier = 
			"""[a-zA-Z]+""".r ^^ { id => Id(Symbol(id)) }

		override def value: Parser[Value] = 
			natValue | leftValue | rightValue | unitValue | 
			tupleValue | funcValue | identifier | "(" ~> application <~ ")"

		def application: Parser[Value] = 
			rep1(value) ^^ { 
				case vs => 
					vs.tail.foldLeft(vs.head) { 
						case (all, el) => 
							App(all, el) 
					}
			}
	}
}
