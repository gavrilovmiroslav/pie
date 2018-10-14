
package edu.ucsb.pllab.pie

import scala.util.parsing.combinator._

package values {
	trait Value { value: Value =>
		def tup(r: Value) = Product(value, r)

		def $(r: Value) = App(value, r)
	}

	case object One extends Value
	case class N(n: Int) extends Value
	case class Id(id: Symbol) extends Value
	case class Left(v: Value) extends Value
	case class Right(v: Value) extends Value
	case class Product(l: Value, r: Value) extends Value
	case class Func(v: Value, body: Value) extends Value
	case class App(l: Value, r: Value) extends Value 

	object ValueLevel {
		def one = One
		def id(s: Symbol) = Id(s)
		def nat(n: Int) = N(n)
		def left(v: Value) = Left(v)
		def right(v: Value) = Right(v)
		def prod(l: Value, r: Value) = Product(l, r)
		def func(v: Value, b: Value) = Func(v, b)
		def $(l: Value, r: Value) = App(l, r)

		object Implicits {
			import scala.language.implicitConversions
			implicit def SymToId(s: Symbol): Id = id(s)
		}
	}

	trait ValueParser { this: RegexParsers =>

		def natValue: Parser[N] = 
			"""0|[1-9][0-9]*""".r ^^ { n => N(n.toInt) }
		
		def unitValue: Parser[Value] = 
			"_".r ^^ { _ => One }
		
		def leftValue: Parser[Left] = 
			"left" ~> application ^^ { case v => Left(v) }
		
		def rightValue: Parser[Right] = 
			"right" ~> application ^^ { case v => Right(v) }
		
		def tupleValue: Parser[Product] = 
			"(" ~> application ~ "," ~ application <~ ")" ^^ { case l ~ _ ~ r => Product(l, r) }

		def funcValue: Parser[Func] = 
			"λ" ~> value ~ "." ~ application ^^ { case v ~ _ ~ b => Func(v, b) }

		def identifier = 
			"""[a-zA-Z]+""".r ^^ { id => Id(Symbol(id)) }

		def value: Parser[Value] = 
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
