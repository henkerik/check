package nl.henkerikvanderhoek.example

import nl.henkerikvanderhoek.check.Check

object Reverse extends App {
  def propReverseUnit[A]: A => Boolean =
    x => List(x).reverse == List(x)

  def propReverseAppend[A]: List[A] => List[A] => Boolean =
    xs => ys => (xs ++ ys).reverse == ys.reverse ++ xs.reverse

  def propDoubleReverse[A]:List[A] => Boolean =
    xs => xs.reverse.reverse == xs

  Check(propReverseUnit: Int => Boolean)
  Check(propReverseAppend: List[Int] => List[Int] => Boolean)
  Check(propDoubleReverse: List[Int] => Boolean)
}