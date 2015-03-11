package nl.henkerikvanderhoek.check

sealed trait Result {
  def addArg(argument: Any): Result = this match {
    case Success(arguments) => Success(argument :: arguments)
    case Failure(arguments) => Failure(argument :: arguments)
  }
}
case class Success(arguments: List[Any]) extends Result
case class Failure(arguments: List[Any]) extends Result

object Result {
  def apply(b:Boolean):Result =
    if (b) { Success(List.empty) }
    else   { Failure(List.empty) }
}

case class Property (result: Gen[Result])

object Property {
  def apply(b:Boolean):Property =
    new Property(Gen(Result(b)))
}

trait Testable[A] {
  def property(a:A):Property
}

object Testable {
  def apply[A:Testable]:Testable[A] = implicitly[Testable[A]]

  implicit def TestableBoolean = new Testable[Boolean] {
    def property(b: Boolean):Property = Property (b)
  }

  implicit def TestableFunction[A:Arbitrary, B:Testable] = new Testable[A => B] {
    def property(f: A => B):Property = Property {
      for {
        a <- Arbitrary[A].arbitrary
        r <- Testable[B].property(f(a)).result
      } yield r.addArg (a)
    }
  }
}