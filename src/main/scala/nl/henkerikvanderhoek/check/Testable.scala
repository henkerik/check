package nl.henkerikvanderhoek.check

sealed trait Result {
  def addArgument(argument: String): Result = this match {
    case Success(arguments) => Success(argument :: arguments)
    case Failure(arguments) => Failure(argument :: arguments)
  }
}
case class Success(arguments: List[String]) extends Result
case class Failure(arguments: List[String]) extends Result

case class Property (result: Gen[Result])

object Property {
  def apply(b:Boolean):Property = {
    new Property(Gen {
      if (b) {
        Success(List.empty)
      } else {
        Failure(List.empty)
      }
    })
  }
}

trait Testable[A] {
  def property(a:A):Property
}

object Testable {
  def apply[A](implicit e: Testable[A]): Testable[A] = e

  def forAll[A:Showable,B:Testable](gen:Gen[A])(f: A => B):Property = Property {
    for {
      a <- gen
      r <- Testable[B].property(f(a)).result
    } yield r.addArgument(Showable[A].show(a))
  }

  implicit def TestableProperty = new Testable[Property] {
    def property(p:Property):Property = p
  }

  implicit def TestableBoolean = new Testable[Boolean] {
    def property(b: Boolean):Property = Property (b)
  }

  implicit def TestableFunction[A:Arbitrary:Showable, B:Testable] = new Testable[A => B] {
    def property(f: A => B):Property = forAll(Arbitrary[A].arbitrary)(f)
  }
}