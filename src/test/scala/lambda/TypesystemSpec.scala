
package edu.ucsb.pllab.pie.lambda
import edu.ucsb.pllab.pie._
import org.scalatest._
import scala.util.parsing.combinator._

class TypesystemLevelParserSpec extends FlatSpec with Matchers {

	import types._
	import values._

	import TypeLevel._
	import ValueLevel._
	import λTypeLevel._
	import λTypeLevel.Implicits._
	import λValueLevel._

	import TypesystemLevel._

	object Parser extends RegexParsers 
		with λTypesystemParser 
		with ConcreteParser[Value] {
		
		override def main = application
	}
	
	"The typesystem" should "allow only typed arguments in functions" in {
		assert(Parser("λx.left x").isNo)
		assert(Parser("λ_:1._").isNo)
		assert(Parser("λf. λg. f g").isNo)

		assert(Parser("λx:1._") == 
			Yes(func(τd(id('x), τU), one)))

		assert(Parser("λx:'t+'u.left x") == 
			Yes(func(τd(id('x), 't+'u), left(id('x)))))
	
		assert(Parser("λf:'a⟶'b. λg:'a. f g") ==
			Yes(func(τd(id('f), 'a ⟶ 'b), func(τd(id('g), 'a), id('f) $ id('g)))))

		assert(Parser("λ3:'a.3").isNo)
		assert(Parser("λ'a:'a.3").isNo)
	}

	"Typechecking" should "work with simple types" in {	
		assert(typecheck(Map())(Parser("_").get) == Yes(τU))
		assert(typecheck(Map((id('x), τN)))(Parser("x").get) == Yes(τN))
		assert(typecheck(Map())(Parser("x").get).isNo)
		assert(typecheck(Map())(Parser("3").get) == Yes(τN))
		assert(typecheck(Map())(Parser("(2, 3)").get) == Yes(τN × τN))
		assert(typecheck(Map())(Parser("(1, _)").get) == Yes(τN × τU))
		assert(typecheck(Map())(Parser("(_, 4)").get) == Yes(τU × τN))	
		assert(typecheck(Map())(Parser("left 3").get) == Yes(τN + τInf))
		assert(typecheck(Map())(Parser("right 5").get) == Yes(τInf + τN))
		assert(typecheck(Map())(Parser("(λf:Nat.f) 23").get) == Yes(τN))
		assert(typecheck(Map())(Parser("(λf:Nat × Nat.f) (2, 3)").get) == Yes(τN × τN))
	}

	it should "enable type introduction through function abstractions" in {	
		assert(typecheck(Map())(Parser("λx:'a.3").get) == Yes('a⟶τN))
		assert(typecheck(Map())(Parser("λx:'a.x").get) == Yes('a⟶'a))
		assert(typecheck(Map())(Parser("λf:'a⟶'b.λg:'a.f g").get) == Yes(('a⟶'b)⟶('a⟶'b)))
	}

	it should "be aware of shadowing" in {	
		assert(typecheck(Map())(Parser("λf:'a⟶'b.λf:'a.f").get) == Yes(('a⟶'b)⟶('a⟶'a)))
		assert(typecheck(Map())(Parser("λf:'a⟶'b.λg:'a.f").get) == Yes(('a⟶'b)⟶('a⟶('a⟶'b))))
	}

	it should "stop the application of weird arguments" in {	
		assert(typecheck(Map())(Parser("(λf:Nat.f) _").get).isNo)
		assert(typecheck(Map())(Parser("(λf:Nat × Nat.f) _").get).isNo)
	}

	"Inference" should "work for sum types" in {	
		assert(typecheck(Map())(Parser("(λf:1+1._) left _").get) == Yes(τU))
		assert(typecheck(Map())(Parser("(λf:1+1.f) left _").get) == Yes(τU + τU))
	}
}