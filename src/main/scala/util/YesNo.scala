
package edu.ucsb.pllab.pie

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