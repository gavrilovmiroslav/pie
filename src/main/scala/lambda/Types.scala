
import scala.util.parsing.combinator._
import edu.ucsb.pllab.pie._

package edu.ucsb.pllab.pie.types {

	case class FuncType(l: Type, r: Type) extends Type
	case class VarType(name: Symbol) extends StandardType

	object λTypeLevel {
		object Implicits {
			import scala.language.implicitConversions
			implicit def SymToVar(s: Symbol): Type = τV(s)

			implicit class FunctionalType(t: Type) {
				def ⟶(r: Type) = FuncType(t, r)
			}

			implicit class FunctionalTypeFromSym(t: Symbol) {
				def ⟶(r: Type) = FuncType(τV(t), r)
			}

		}

		def τV(name: Symbol) = VarType(name)
	}

	trait λTypeParser extends TypeParser { this: RegexParsers =>

 		def τFunc: Parser[FuncType] =
 			τStandard ~ "⟶" ~ τ ^^ { case (l ~ _ ~ r) => FuncType(l, r) }

		def τVar: Parser[StandardType] = 
			"'[A-Za-z][a-zA-Z0-9_]*".r ^^ { id => VarType(Symbol(id.substring(1))) }

		override def τStandard: Parser[Type] = 
			τVoid | τUnit | τNat | τVar | "(" ~> τ <~ ")"
		
		override def τ: Parser[Type] = 
			τSum | τProduct | τFunc | τStandard
	}
}

