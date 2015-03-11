package nl.henkerikvanderhoek.example

import nl.henkerikvanderhoek.check.Check

object Sort extends App {

  def sort[A:Ordering](xs: List[A]):List[A] = xs match {
    case Nil     => Nil
    case (x::xs) => {
      val lower  = sort(xs.filter { y => Ordering[A].lt(y,x) })
      val higher = sort(xs.filter { y => Ordering[A].gteq(y,x) })

      lower ++ List(x) ++ higher
    }
  }

  def propSortAscending[A:Ordering]:List[A] => Boolean =
    xs => sorted(sort(xs))

  def propKeepValues[A:Ordering]:List[A] => Boolean = xs =>
    sort(xs).foldLeft[Option[List[A]]](Some(xs)) {
      case (None,      x) => None
      case (Some(rem), x) => remove(rem)(x)
    } == Some(List.empty)


  def propKeepLength[A:Ordering]:List[A] => Boolean =
    xs => sort(xs).length == xs.length


  Check(propSortAscending: List[Int] => Boolean)
  Check(propKeepValues:    List[Int] => Boolean)
  Check(propKeepLength:    List[Int] => Boolean)

  private def sorted[A:Ordering](xs:List[A]):Boolean = xs match {
    case Nil      => true
    case x::Nil   => true
    case x::y::ys => Ordering[A].lteq(x, y) && sorted(y::ys)
  }

  private def remove[A](xs:List[A])(y:A):Option[List[A]] = {
    def go(xs: List[A], ys: List[A]):Option[List[A]] = xs match {
      case Nil   => None
      case x::xs => if (x == y) { Some(ys.reverse ++ xs) } else { go(xs, x::ys) }
    }
    go(xs, List.empty)
  }
}