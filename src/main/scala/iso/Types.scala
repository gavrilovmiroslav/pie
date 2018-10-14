
import scala.util.parsing.combinator._
import edu.ucsb.pllab.pie._

package edu.ucsb.pllab.pie.types {
    import TypeLevel._

    trait CombType extends Type
    class UniType(name: String, forward: Seq[Type] => Type) extends CombType {
        override def toString = name
    }

    trait IsoType extends CombType {
        def elim: UniType
        def intr: UniType
    }

    // todo
    case object Arith extends IsoType {
        case object ArithE extends UniType("inc", (b: Seq[Type]) => b(0))
        case object ArithI extends UniType("dec", (b: Seq[Type]) => b(0))
        override def elim = ArithE
        override def intr = ArithI      
    }

    case object ZeroPlus extends IsoType {
        case object ZeroPlusE extends UniType("zeroe+", (b: Seq[Type]) => τV + b(0))
        case object ZeroPlusI extends UniType("zeroi+", (b: Seq[Type]) => b(0))
        override def elim = ZeroPlusE
        override def intr = ZeroPlusI
    }

    case object SwapPlus extends IsoType {
        case object SwapPlusE extends UniType("swap+", (b: Seq[Type]) => b(0) + b(1))
        case object SwapPlusI extends UniType("swap+", (b: Seq[Type]) => b(1) + b(0))
        override def elim = SwapPlusE
        override def intr = SwapPlusI
    }

    case object AssocPlus extends IsoType {
        case object AssocPlusE extends UniType("assocl+", (b: Seq[Type]) => b(0) + (b(1) + b(2)))
        case object AssocPlusI extends UniType("assocr+", (b: Seq[Type]) => (b(0) + b(1)) + b(2))
        override def elim = AssocPlusE
        override def intr = AssocPlusI
    }

    case object Unitary extends IsoType {
        case object UnitaryE extends UniType("unite", (b: Seq[Type]) => τU × b(0))
        case object UnitaryI extends UniType("uniti", (b: Seq[Type]) => b(0))
        override def elim = UnitaryE
        override def intr = UnitaryI
    }

    case object SwapTimes extends IsoType {
        case object SwapTimesE extends UniType("swap×", (b: Seq[Type]) => b(0) × b(1))
        case object SwapTimesI extends UniType("swap×", (b: Seq[Type]) => b(1) × b(0))
        override def elim = SwapTimesE
        override def intr = SwapTimesI
    }

    case object AssocTimes extends IsoType {
        case object AssocTimesE extends UniType("assocl×", (b: Seq[Type]) => b(0) × (b(1) × b(2)))
        case object AssocTimesI extends UniType("assocr×", (b: Seq[Type]) => (b(0) × b(1)) × b(2))
        override def elim = AssocTimesE
        override def intr = AssocTimesI
    }

    case object ZeroTimes extends IsoType {
        case object ZeroTimesE extends UniType("zeroe×", (b: Seq[Type]) => τV × b(0))
        case object ZeroTimesI extends UniType("zeroi×", (b: Seq[Type]) => τV)
        override def elim = ZeroTimesE
        override def intr = ZeroTimesI
    }

    case object Distrib extends IsoType {
        case object DistribE extends UniType("distrib", (b: Seq[Type]) => (b(0) + b(1)) × b(2))
        case object DistribI extends UniType("factor", (b: Seq[Type]) => (b(0) × b(2)) + (b(1) × b(2)))
        override def elim = DistribE
        override def intr = DistribI
    }

    case object Id extends CombType {
        override def toString = "id"
    }

    case class Sym(b: CombType) extends CombType {
        override def toString = s"sym $b"
    }

    case class Then(b1: CombType, b2: CombType) extends CombType {
        override def toString = s"$b1; $b2"
    }

    case class ΣIso(b1: CombType, b2: CombType) extends CombType {
        override def toString = s"($b1 + $b2)"
    }

    case class ΠIso(b1: CombType, b2: CombType) extends CombType {
        override def toString = s"($b1 × $b2)"
    }

    trait DebugRegexParsers extends RegexParsers {
        class Wrap[+T](name:String,parser:Parser[T]) extends Parser[T] {
            def apply(in: Input): ParseResult[T] = {
                val first = in.first
                val pos = in.pos
                val offset = in.offset
                val t = parser.apply(in)
                println(name+".apply for token "+first+
                        " at position "+pos+" offset "+offset+" returns "+t)
                t
            }
        }
    }

    trait πTypeParser { this: DebugRegexParsers => 
        def inc: Parser[UniType] = "inc" ^^ { _ => Arith.elim }
        def dec: Parser[UniType] = "dec" ^^ { _ => Arith.intr }

        def zeroePlus: Parser[UniType] = "zeroe+" ^^ { _ => ZeroPlus.elim }
        def zeroiPlus: Parser[UniType] = "zeroi+" ^^ { _ => ZeroPlus.intr }

        def swapPlus: Parser[UniType] = "swap+" ^^ { _ => SwapPlus.elim }
        def swapTimes: Parser[UniType] = "swap×" ^^ { _ => SwapTimes.elim }

        def assoclPlus: Parser[UniType] = "assocl+" ^^ { _ => AssocPlus.elim }
        def assocrPlus: Parser[UniType] = "assocr+" ^^ { _ => AssocPlus.intr }

        def unite: Parser[UniType] = "unite" ^^ { _ => Unitary.elim }
        def uniti: Parser[UniType] = "uniti" ^^ { _ => Unitary.intr }

        def assoclTimes: Parser[UniType] = "assocl×" ^^ { _ => AssocTimes.elim }
        def assocrTimes: Parser[UniType] = "assocr×" ^^ { _ => AssocTimes.intr }

        def zeroeTimes: Parser[UniType] = "zeroe×" ^^ { _ => ZeroTimes.elim }
        def zeroiTimes: Parser[UniType] = "zeroi×" ^^ { _ => ZeroTimes.intr }

        def distrib: Parser[UniType] = "distrib" ^^ { _ => Distrib.elim }
        def factor: Parser[UniType] = "factor" ^^ { _ => Distrib.intr }

        def cThen: Parser[CombType] =
             iso ~ ";" ~ comb ^^ { case (a ~ _ ~ b) => Then(a, b) }

        def cSumIso: Parser[CombType] =
             iso ~ "+" ~ comb ^^ { case (a ~ _ ~ b) => ΣIso(a, b) }

        def cProdIso: Parser[CombType] =
            iso ~ "×" ~ comb ^^ { case (a ~ _ ~ b) => ΠIso(a, b) }

        def iso: Parser[CombType] = 
            inc | dec | zeroePlus | zeroiPlus | swapPlus | swapTimes | 
            assoclPlus | assocrPlus | unite | uniti | assoclTimes | 
            assocrTimes | zeroeTimes | zeroiTimes | distrib | factor | 
            cId | cSym | "(" ~> comb <~ ")" | comb

        def cId: Parser[CombType] = 
            "id" ^^ { _ => Id }

        def cSym: Parser[CombType] = 
            "sym" ~> iso ^^ { case c => Sym(c) }

        def comb: Parser[CombType] = 
            cThen | cSumIso | cProdIso | iso
    }
}
