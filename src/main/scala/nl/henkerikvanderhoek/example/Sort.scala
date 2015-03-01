package nl.henkerikvanderhoek.example

import nl.henkerikvanderhoek.check.Check

object Sort extends App {

  def sort[A:Ordering](xs: List[A]):List[A] = xs match {
    case Nil     => Nil
    case (x::xs) => {
      val lower  = sort(xs.filter { y => Ordering[A].lt(x, y) })
      val higher = sort(xs.filter { y => Ordering[A].gt(x, y) })

      lower ++ List(x) ++ higher
    }
  }

  def sorted[A:Ordering](xs:List[A]):Boolean = xs match {
    case Nil      => true
    case x::Nil   => true
    case x::y::ys => Ordering[A].lteq(x, y) && sorted(y::ys)
  }

  def propSortAscending[A:Ordering]:List[A] => Boolean =
    xs => sorted(sort(xs))

  def propKeepValues[A:Ordering]:List[A] => Boolean =
    xs => xs.permutations.contains(sort(xs))

  def propKeepLength[A:Ordering]:List[A] => Boolean =
    xs => sort(xs).length == xs.length


  Check(propSortAscending: List[Int] => Boolean)
  Check(propKeepValues:    List[Int] => Boolean, 100)
  Check(propKeepLength:    List[Int] => Boolean)
}