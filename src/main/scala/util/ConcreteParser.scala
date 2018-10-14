
package edu.ucsb.pllab.pie

import scala.util.parsing.combinator._

trait ConcreteParser[T] { this: RegexParsers =>
	def main: this.Parser[T]

	def apply(q: String) = this.parseAll(main, q) match {
		case Success(matching, _) => Yes(matching)
		case Failure(err, _) => No(err)
		case Error(err, _) => No(err)
	}
}
