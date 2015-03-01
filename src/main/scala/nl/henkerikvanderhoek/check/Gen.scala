package nl.henkerikvanderhoek.check

case class Gen[A](run: RNG => (A,RNG)) {
  def map[B](f: A => B):Gen[B] = Gen { rng =>
    run(rng) match { case (a,next) => (f(a), next) }
  }

  def flatMap[B](f: A => Gen[B]):Gen[B] = Gen { rng =>
    run(rng) match { case (a,next) => f(a).run(next) }
  }
}

object Gen {
  def apply[A](a:A):Gen[A] = Gen { rng => (a, rng) }

  def choose(min: Int, max: Int):Gen[Int] =
    probability.map { r => Math.round(r * (max - min)).toInt + min}

  def elements[A](xs:List[A]):Gen[A] =
    choose(0, xs.length - 1).map { index => xs(index) }

  def oneOf[A](xs:List[Gen[A]]):Gen[A] =
    elements(xs).flatMap { x => x }

  def frequency[A](xs:List[(Int,Gen[A])]):Gen[A] =
    oneOf { xs.map { case (n, gen) => List.fill(n)(gen) }.flatten }

  private val unsigned:Gen[Int] =
    Gen { rng =>  rng.nextInt }

  private val signed:Gen[Int] =
    unsigned.map { i => if (i < 0) -(i + 1) else i }

  private val probability:Gen[Double] =
    signed.map { i => i / (Int.MaxValue.toLong + 1) }
}
