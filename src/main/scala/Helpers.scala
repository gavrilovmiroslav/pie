
package edu.ucsb.pllab.pie

import scala.util.parsing.combinator._

trait ParserHelper[T] { this: RegexParsers =>
	def main: this.Parser[T]

	def apply(q: String) = this.parseAll(main, q) match {
		case Success(matching, _) => scala.util.Success(matching)
		case Failure(err, _) => scala.util.Failure(new Exception(err))
		case Error(err, _) => scala.util.Failure(new Exception(err))
	}
}

trait YesNo[+T] {
	def get: T
	def isYes: Boolean
	def isNo: Boolean
}

case class Yes[+T](t: T) extends YesNo[T] {
	override def get = t
	override def isYes = true
	override def isNo = false
}

case class No(s: String) extends YesNo[Nothing] {
	override def get = throw new Exception("Can't extract data from a `No`.")
	override def isYes = false
	override def isNo = true
}