
package edu.ucsb.pllab.pie

import scala.util.parsing.combinator._

package values {
	trait Value { value: Value =>
		def tup(r: Value) = Product(value, r)
	}

	case object One extends Value {
		override def toString = "_"
	}

	case class N(n: Int) extends Value {
		override def toString = s"$n"
	}

	case class Left(v: Value) extends Value {
		override def toString = s"left $v"
	}

	case class Right(v: Value) extends Value {
		override def toString = s"right $v"
	}

	case class Product(l: Value, r: Value) extends Value {
		override def toString = s"($l, $r)"
	}

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
