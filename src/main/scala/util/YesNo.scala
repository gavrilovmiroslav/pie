
package edu.ucsb.pllab.pie

sealed trait YesNo[+T] {
	def get: T
	def isYes: Boolean
	def isNo: Boolean

  def map[R](f: T => R): YesNo[R]
  def flatMap[R](f: T => YesNo[R]): YesNo[R] = f(this.get)
  def withFilter(f: YesNo[T] => Boolean): YesNo[T] = this
  def foreach[U](f: T => U)
}

case class Yes[+T](t: T) extends YesNo[T] {
	override def get = t
	override def isYes = true
	override def isNo = false

  def map[R](f: T => R): YesNo[R] = Yes(f(t))
  def foreach[U](f: T => U) = f(t)
}

case class No(s: String) extends YesNo[Nothing] {
	override def get = throw new Exception("Can't extract data from a `No`.")
	override def isYes = false
	override def isNo = true

  def map[R](f: Nothing => R): YesNo[R] = No(s)
  def foreach[U](f: Nothing => U) = {}
}
