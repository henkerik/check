package nl.henkerikvanderhoek.check

trait Arbitrary[A] {
  def arbitrary:Gen[A]
}

object Arbitrary {
  def apply[A](implicit e:Arbitrary[A]):Arbitrary[A] = e

  implicit def ArbitraryChar = new Arbitrary[Char] {
    def arbitrary:Gen[Char] = Gen.elements { ('a' until 'z').toList }
  }

  implicit def ArbitraryBoolean = new Arbitrary[Boolean] {
    def arbitrary:Gen[Boolean] = Gen.elements (List(true,false))
  }

  implicit def ArbitraryInt = new Arbitrary[Int] {
    def arbitrary:Gen[Int] = Gen.choose(-100, 100)
  }

  implicit def ArbitraryList[A:Arbitrary] = new Arbitrary[List[A]] {
    def arbitrary:Gen[List[A]] = Gen.frequency (List(
      (1, Gen(List.empty)),
      (7, for { a <- Arbitrary[A].arbitrary; as <- arbitrary } yield a::as)
    ))
  }

  implicit def ArbitrarySet[A:Arbitrary] = new Arbitrary[Set[A]] {
    def arbitrary:Gen[Set[A]] = Arbitrary[List[A]].arbitrary.map { xs => xs.toSet }
  }
}