
import scala.util.parsing.combinator._
import edu.ucsb.pllab.pie._

package edu.ucsb.pllab.pie.iso {

	trait Context
	case object Empty extends Context
	case class Fst(ctx: Context, c: types.CombType) extends Context {
		override def toString = s"(Fst $ctx $c)"
	}

	case class Snd(c: types.CombType, ctx: Context) extends Context {
		override def toString = s"(Snd $c $ctx)"		
	}

	case class LTimes(ctx: Context, c: types.CombType, v: values.Value) extends Context {
		override def toString = s"(L× $ctx $c $v)"
	}

	case class RTimes(c: types.CombType, v: values.Value, ctx: Context) extends Context {
		override def toString = s"(R× $c $v $ctx)"
	}

	case class LPlus(ctx: Context, c: types.CombType) extends Context {
		override def toString = s"(L+ $ctx $c)"		
	}

	case class RPlus(c: types.CombType, ctx: Context) extends Context {
		override def toString = s"(R+ $c $ctx)"		
	}
	
	trait State {
		def get: values.Value
	}

	case class Soft(c: types.CombType, v: values.Value, ctx: Context) extends State {
		override def toString = s"⟨ $c, $v, $ctx ⟩"

		override def get = v
	}

	case class Hard(c: types.CombType, v: values.Value, ctx: Context) extends State {
		override def toString = s"[ $c, $v, $ctx ]"

		override def get = v
	}

	object VM {
		import types._
		import values._

		def evalPrimitive(iso: CombType, v: Value): YesNo[Value] = (iso, v) match {
			case (types.Id, whatever) => Yes(whatever)

			case (Arith.ArithE, N(n)) => Yes(N(n + 1))
			case (Arith.ArithI, N(n)) => Yes(N(n - 1))
			case (Arith.ArithE, what) => No(s"Can't increment $what.")
			case (Arith.ArithI, what) => No(s"Can't decrement $what.")

			case (SwapPlus.SwapPlusE, Left(v)) => Yes(Right(v))
			case (SwapPlus.SwapPlusE, Right(v)) => Yes(Left(v))
			case (SwapPlus.SwapPlusI, Left(v)) => Yes(Right(v))
			case (SwapPlus.SwapPlusI, Right(v)) => Yes(Left(v))

			case (AssocPlus.AssocPlusE, Left(v)) => Yes(Left(Left(v)))
			case (AssocPlus.AssocPlusE, Right(Left(v))) => Yes(Left(Right(v)))
			case (AssocPlus.AssocPlusE, Right(Right(v))) => Yes(Right(v))

			case (AssocPlus.AssocPlusI, Left(Left(v))) => Yes(Left(v))
			case (AssocPlus.AssocPlusI, Left(Right(v))) => Yes(Right(Left(v)))
			case (AssocPlus.AssocPlusI, Right(v)) => Yes(Right(Right(v)))

			case (Unitary.UnitaryE, Product(One, v)) => Yes(v)
			case (Unitary.UnitaryI, v) => Yes(Product(One, v))

			case (SwapTimes.SwapTimesE, Product(v1, v2)) => Yes(Product(v2, v1))
			case (SwapTimes.SwapTimesI, Product(v1, v2)) => Yes(Product(v2, v1))

			case (AssocTimes.AssocTimesE, Product(v1, Product(v2, v3))) => Yes(Product(Product(v1, v2), v3))
			case (AssocTimes.AssocTimesI, Product(Product(v1, v2), v3)) => Yes(Product(v1, Product(v2, v3)))

			case (Distrib.DistribE, Product(Left(v1), v2)) => Yes(Left(Product(v1, v2)))
			case (Distrib.DistribE, Product(Right(v1), v2)) => Yes(Right(Product(v1, v2)))

			case (Distrib.DistribI, Left(Product(v1, v2))) => Yes(Product(Left(v1), v2))
			case (Distrib.DistribI, Right(Product(v1, v2))) => Yes(Product(Right(v1), v2))

			case _ => No(s"Couldn't evaluate isomorphic value:\n\t$iso $v")
		}

		def step(state: State): YesNo[(Int, State)] = state match {
			case Soft(iso: UniType, v, ctx) =>
				evalPrimitive(iso, v) match {
					case Yes(v1) => Yes((1, Hard(iso, v1, ctx)))
					case no@No(_) => no
				}

			case Soft(types.Id, v, ctx) =>
				Yes((1, Hard(types.Id, v, ctx)))

			case Soft(Then(c1, c2), v, ctx) => 
				Yes((2, Soft(c1, v, Fst(ctx, c2))))

			case Hard(c1, v, Fst(ctx, c2)) =>
				Yes((3, Soft(c2, v, Snd(c1, ctx))))

			case Hard(c2, v, Snd(c1, ctx)) =>
				Yes((4, Hard(Then(c1, c2), v, ctx)))

			case Soft(ΣIso(c1, c2), Left(v), ctx) =>
				Yes((5, Soft(c1, v, LPlus(ctx, c2))))

			case Hard(c1, v, LPlus(ctx, c2)) =>
				Yes((6, Hard(ΣIso(c1, c2), Left(v), ctx)))

			case Soft(ΣIso(c1, c2), Right(v), ctx) =>
				Yes((7, Soft(c2, v, RPlus(c1, ctx))))

			case Hard(c2, v, RPlus(c1, ctx)) =>
				Yes((8, Hard(ΣIso(c1, c2), Right(v), ctx)))

			case Soft(ΠIso(c1, c2), Product(v1, v2), ctx) =>
				Yes((9, Soft(c1, v1, LTimes(ctx, c2, v2))))

			case Hard(c1, v1, LTimes(ctx, c2, v2)) =>
				Yes((10, Soft(c2, v2, RTimes(c1, v1, ctx))))

			case Hard(c2, v2, RTimes(c1, v1, ctx)) =>
				Yes((11, Hard(ΠIso(c1, c2), Product(v1, v2), ctx)))

			case end@Hard(c, v, Empty) => 
				Yes((0, end))

			case _ => No(s"Couldn't progress state:\n\t$state")
		}

		def trace(program: CombType, v: Value): YesNo[Seq[(Int, State)]] = {
			val start: State = Soft(program, v, Empty)

			import scala.annotation.tailrec

			@tailrec
			def fixpoint(state: State, last: YesNo[Seq[(Int, State)]]): YesNo[Seq[(Int, State)]] = {
				step(state) match {
					case No(msg) =>
						No(msg + "\n\t" + last.get.mkString("\n\t"))

					case Yes((n, next)) =>
						val current = Yes((last.get :+ (n, next)))
						if(next == state) 
							current
						else
							fixpoint(next, current)
				}
			}

			fixpoint(start, Yes(Seq()))
		}

		def eval(program: CombType, v: Value): YesNo[Value] = 
			trace(program, v) match {
				case Yes(traced) => Yes(traced.last._2.get)
				case No(err) => No(err)
			}

	}
}