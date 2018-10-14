
package edu.ucsb.pllab.pie

import scala.util.parsing.combinator._

package values {
	trait Value { value: Value =>
		def tup(r: Value) = Product(value, r)
		def $(r: Value) = App(value, r)
	}

	case object One extends Value
	case class N(n: Int) extends Value
	case class Left(v: Value) extends Value
	case class Right(v: Value) extends Value
	case class Product(l: Value, r: Value) extends Value
	case class App(l: Value, r: Value) extends Value 

	object ValueLevel {
		def one = One
		def id(s: Symbol) = Id(s)
		def nat(n: Int) = N(n)
		def left(v: Value) = Left(v)
		def right(v: Value) = Right(v)
		def prod(l: Value, r: Value) = Product(l, r)
	}

	trait ValueParser { this: RegexParsers =>

		def natValue: Parser[N] = 
			"""0|[1-9][0-9]*""".r ^^ { n => N(n.toInt) }
		
		def unitValue: Parser[Value] = 
			"_".r ^^ { _ => One }
		
		def leftValue: Parser[Left] = 
			"left" ~> value ^^ { case v => Left(v) }
		
		def rightValue: Parser[Right] = 
			"right" ~> value ^^ { case v => Right(v) }
		
		def tupleValue: Parser[Product] = 
			"(" ~> value ~ "," ~ value <~ ")" ^^ { case l ~ _ ~ r => Product(l, r) }

		def value: Parser[Value] = 
			natValue | leftValue | rightValue | unitValue | 
			tupleValue | "(" ~> value <~ ")"
	}
}
