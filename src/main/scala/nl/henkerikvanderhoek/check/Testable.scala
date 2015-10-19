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

trait Testable[A] {
  def test(a:A):Gen[Result]
}

object Testable {
  def apply[A:Testable]:Testable[A] = implicitly[Testable[A]]

  implicit def TestableBoolean = new Testable[Boolean] {
    def test(b: Boolean):Gen[Result] = Gen(Result(b))
  }

  implicit def TestableFunction[A:Arbitrary, B:Testable] = new Testable[A => B] {
    def test(f: A => B):Gen[Result] = for {
        a <- Arbitrary[A].arbitrary
        r <- Testable[B].test(f(a))
      } yield r.addArg (a)
  }
}