package nl.henkerikvanderhoek.example

import nl.henkerikvanderhoek.check._

object Substitutions extends App {

  type VarName = String

  case class Subst (xs:List[(VarName,Expr)]) {
    def lookup(k:VarName):Option[Expr] =
      xs.find{ case (a,b) => k == a }.map { case (a,b) => b }

    def compose (subst:Subst):Subst =
      Subst { subst.xs.map { case (v,e) => (v,e.apply(this)) } ++ xs }
  }

  sealed trait Expr {
    def apply(subst:Subst):Expr = this match {
      case Var(x)         => subst.lookup(x).fold[Expr](Var(x))(identity)
      case Atom(a)        => Atom(a)
      case Number(n)      => Number(n)
      case Compound(f,xs) => Compound(f,xs.map { expr => expr.apply(subst) })
    }
  }
  case class Var(x:VarName) extends Expr
  case class Atom(a:String) extends Expr
  case class Number(n:Int) extends Expr
  case class Compound(f:String, xs:List[Expr]) extends Expr

  implicit def ArbitraryVar = new Arbitrary[Var] {
    def arbitrary:Gen[Var] = Gen.elements(List("A","B","C","D")).map { x => Var(x) }
  }

  implicit def ArbitraryAtom = new Arbitrary[Atom] {
    def arbitrary:Gen[Atom] = Gen.elements(List("a","b","c","d")).map { a => Atom(a) }
  }

  implicit def ArbitraryNumber = new Arbitrary[Number] {
    def arbitrary:Gen[Number] = Gen.choose(1, 10).map { n => Number (n) }
  }

  object Expr {
    implicit def ArbitraryExpr = new Arbitrary[Expr] {
      def arbitrary: Gen[Expr] = {
        def go(depth: Int): Gen[Expr] = {
          val compounds = for {
            f <- Gen.elements(List("f", "g", "h"))
            xs <- Gen.repeat(Gen.choose(1, 3))(go(depth - 1))
          } yield Compound(f, xs)

          val xs: List[Gen[Expr]] = List[Gen[Expr]](
            Arbitrary[Var].arbitrary,
            Arbitrary[Atom].arbitrary,
            Arbitrary[Number].arbitrary
          ) ++ (if (depth > 0) List(compounds):List[Gen[Expr]] else List.empty)

          Gen.oneOf(xs)
        }

        go(3)
      }
    }
  }

  object Subst {
    implicit def ArbitrarySubst = new Arbitrary[Subst] {
      def arbitrary: Gen[Subst] = for {
        xs <- Gen.repeat(Gen.choose(1, 3)) {
          for {
            v <- Arbitrary[Var].arbitrary
            e <- Arbitrary[Expr].arbitrary
          } yield (v.x, e)
        }
      } yield Subst(xs)
    }
  }

  def property: Expr => Subst => Subst => Boolean = expr => s1 => s2 =>
    expr.apply(s1 compose s2) == expr.apply(s2).apply(s1)

  Check(property)
}
