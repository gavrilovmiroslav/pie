
package edu.ucsb.pllab.pie

import scala.util.parsing.combinator._

package types {
	trait Type extends values.Value { this: Type =>
		def +(r: Type) = SumType(this, r)
		def ×(r: Type) = ProductType(this, r)
	}

	case class SumType(l: Type, r: Type) extends Type {
		override def toString = s"$l + $r"
	}

	case class ProductType(l: Type, r: Type) extends Type {
		override def toString = s"$l × $r"
	}

	object TypeLevel {
		def τV = VoidType
		def τU = UnitType
		def τN = NatType
	}

	trait StandardType extends Type
	case object VoidType extends StandardType {
		override def toString = "0"
	}

	case object UnitType extends StandardType {
		override def toString = "1"
	}

	case object NatType extends StandardType {
		override def toString = "nat"
	}

	trait TypeParser { this: RegexParsers =>

		def τSum: Parser[Type] = 
			τStandard ~ "+" ~ τ ^^ { case (l ~ _ ~ r) => SumType(l, r) }

		def τProduct: Parser[Type] = 
			τStandard ~ "×" ~ τ ^^ { case (l ~ _ ~ r) => ProductType(l, r) }

		def τVoid: Parser[StandardType] = 
			"0" ^^ { _ => VoidType }

		def τUnit: Parser[StandardType] = 
			"1" ^^ { _ => UnitType }

		def τNat: Parser[StandardType] = 
			"Nat" ^^ { _ => NatType }

		def τStandard: Parser[Type] = 
			τVoid | τUnit | τNat | "(" ~> τ <~ ")"
		
		def τ: Parser[Type] = 
			τSum | τProduct | τStandard
	}
}
