package nl.henkerikvanderhoek.example

import nl.henkerikvanderhoek.check.Check

object Sort extends App {

  def sort[A:Ordering](xs: List[A]):List[A] = xs match {
    case Nil     => Nil
    case (x::xs) => {
      val lower  = sort(xs.filter { y => Ordering[A].lt(y,x) })
      val higher = sort(xs.filter { y => Ordering[A].gt(y,x) })

      lower ++ List(x) ++ higher
    }
  }

  def propSortAscending[A:Ordering]:List[A] => Boolean =
    xs => ???

  Check(propSortAscending: List[Int] => Boolean)

  private def sorted[A:Ordering](xs:List[A]):Boolean = xs match {
    case Nil      => ???
    case x::Nil   => ???
    case x::y::ys => ???
  }
}