
package edu.ucsb.pllab.pie

import scala.util.parsing.combinator._

package typesystem {
	import values._
	import types._

	case class Typed(value: Value, τvalue: Type) extends Value

	trait TypesystemParser extends TypeParser with ValueParser {
		this: RegexParsers =>

		def typed: Parser[Typed] =
			application ~ ":" ~ τ ^^ { case v ~ _ ~ t => Typed(v, t) }

		def typedId: Parser[Typed] =
			identifier ~ ":" ~ τ ^^ { case v ~ _ ~ t => Typed(v, t) }

		override def funcValue: Parser[Func] = 
			"λ" ~> typedId ~ "." ~ application ^^ { case v ~ _ ~ b => Func(v, b) }
	}

	case object Inferable extends Type

	object TypesystemLevel {
		def τInf = Inferable
		def τd(v: Value, τv: Type) = Typed(v, τv)
	}

	object `package` {
		def unify(Γ: Map[Value, Type])(a: Type, b: Type): YesNo[Type] = 
			if(a == Inferable && a == b) 
				No("both types are inferable but no clues given.")
			else if(a == b)
				Yes(a) 
			else if(a == Inferable)
				Yes(b)
			else if(b == Inferable)
				Yes(a)
			else 
				(a, b) match {
					case (SumType(l1, r1), SumType(l2, r2)) =>
						val ul = unify(Γ)(l1, l2)
						if(ul.isNo) return ul 
						val ur = unify(Γ)(r1, r2) 
						if(ur.isNo) return ur 

						Yes(SumType(ul.get, ur.get))

					case (ProductType(l1, r1), ProductType(l2, r2)) =>
						val ul = unify(Γ)(l1, l2)
						if(ul.isNo) return ul 
						val ur = unify(Γ)(r1, r2) 
						if(ur.isNo) return ur 

						Yes(ProductType(ul.get, ur.get))

					case _ => No("couldn't figure out how to unify `$a` and `$b`;\n\tenvironment: $Γ")
				}

		def typecheck(Γ: Map[Value, Type])(ast: Value): YesNo[Type] = ast match {
			case One => 
				Yes(UnitType)

			case N(_) => 
				Yes(NatType)

			case Typed(a, b) =>
				Γ.get(a) match {
					case Some(t) if t == b => Yes(t)
					case Some(t) => 
						No(s"Γ contains `$a` of type `$t`, and not `$b`")
					case None => 
						No(s"Γ does not contain type for `$b`")
				}

			case id@Id(_) =>
				Γ.get(id) match {
					case Some(t) => Yes(t)
					case None => 
						No(s"Γ does not contain type for `$id`")
				}

			case Product(a, b) => 
				(typecheck(Γ)(a), typecheck(Γ)(b)) match {
					case (Yes(l), Yes(r)) => Yes(ProductType(l, r))
					case _ => No(s"Cannot typecheck product `($a, $b)`")
				}

			case Func(Typed(id@Id(a), τa), b) =>
				val env = Γ.get(id) match {
					case Some(ta) if ta == τa => Γ
					case _ => Γ + (id → τa)
				}

				typecheck(env)(b) match {
					case Yes(τb) => Yes(FuncType(τa, τb))
					case _ => No(s"Cannot typecheck function `$a : $τa → $b")
				}

			case Left(a) =>
				typecheck(Γ)(a) match {
					case Yes(l) => Yes(SumType(l, Inferable))
					case _ => No(s"Cannot typecheck left of sum `$a`")
				}

			case Right(b) =>
				typecheck(Γ)(b) match {
					case Yes(r) => Yes(SumType(Inferable, r))
					case _ => No(s"Cannot typecheck right of sum `$b`")
				}

			case App(f, x) =>
				(typecheck(Γ)(f), typecheck(Γ)(x)) match {
					case (Yes(FuncType(a, b)), Yes(c)) =>
						unify(Γ)(a, c) match {
							case Yes(τ) => Yes(b)
							case no@No(_) => no
						}
					case (a, b) => No(s"Cannot typecheck application `($f $x)`\n\ttypeOf(f) := $a\n\ttypeOf(x) := $b\n")
				}

			case x =>
				No(s"Error typechecking $x")
		}

	}
}
