
package edu.ucsb.pllab.pie

import scala.util.parsing.combinator._
package types {
	trait Type extends values.Value { this: Type =>
		def +(r: Type) = SumType(this, r)
		def ×(r: Type) = ProductType(this, r)
		def →(r: Type) = FuncType(this, r)
	}

	case class SumType(l: Type, r: Type) extends Type
	case class ProductType(l: Type, r: Type) extends Type
	case class FuncType(l: Type, r: Type) extends Type

	object TypeLevel {

		def τU = UnitType
		def τN = NatType
		def τV(name: Symbol) = VarType(name)

		object Implicits {
			import scala.language.implicitConversions
			implicit def SymToVar(s: Symbol): Type = τV(s)
		}
	}

	trait StandardType extends Type
	case object UnitType extends StandardType
	case object NatType extends StandardType
	case class VarType(name: Symbol) extends StandardType

	trait TypeParser { this: RegexParsers =>

		def τSum: Parser[Type] = 
			τStandard ~ "+" ~ τ ^^ { case (l ~ _ ~ r) => SumType(l, r) }

		def τProduct: Parser[Type] = 
			τStandard ~ "×" ~ τ ^^ { case (l ~ _ ~ r) => ProductType(l, r) }

		def τFunc: Parser[FuncType] =
			τStandard ~ "→" ~ τ ^^ { case (l ~ _ ~ r) => FuncType(l, r) }

		def τUnit: Parser[StandardType] = 
			"_" ^^ { _ => UnitType }

		def τNat: Parser[StandardType] = 
			"Nat" ^^ { _ => NatType }

		def τVar: Parser[StandardType] = 
			"'[A-Za-z][a-zA-Z0-9_]*".r ^^ { id => VarType(Symbol(id.substring(1))) }

		def τStandard: Parser[Type] = 
			τUnit | τNat | τVar | "(" ~> τ <~ ")"
		
		def τ: Parser[Type] = 
			τSum | τProduct | τFunc | τStandard
	}
}
