package nl.henkerikvanderhoek.check

object Check {
  def apply[A:Testable](testable: A, n: Int = 10000):Unit =
    run(testable, n).run

  def run[A:Testable](testable: A, n:Int, createRNG: Int => RNG = seed => SimpleRNG(seed)):IO[Unit] = {
    val property = Testable[A].property(testable)

    def go(m:Int):IO[Result] = IO.random.flatMap { seed =>
      val (result,rng) = property.result.run(createRNG(seed))
      result match {
        case Success(arguments) => if (m <= 0) { IO(result) } else { go(m - 1) }
        case Failure(arguments) => IO(result)
      }
    }

    go(n).flatMap { result =>
      result match {
        case Success(arguments) => IO.printLine("Successfully passed " + n + " tests")
        case Failure(arguments) => {
          val x = arguments.zipWithIndex.map { case (argument, index) => index + ": " + argument.toString }.mkString("\n")
          IO.printLine("Counter example found: " + x)
        }
      }
    }
  }
}