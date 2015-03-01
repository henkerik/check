package nl.henkerikvanderhoek.check

import scala.util.Random

sealed trait IO[A] {
  def map[B](f: A => B):IO[B] = flatMap(f andThen (Return(_)))

  def flatMap[B](f: A => IO[B]):IO[B] = FlatMap (this, f)

  @annotation.tailrec
  final def run:A = this match {
    case Return(a)     => a
    case Suspend(r)    => r()
    case FlatMap(fb, f) => fb match {
      case Return(b)     => f(b).run
      case Suspend(r)    => f(r()).run
      case FlatMap(y, g) => (y flatMap (a => g(a) flatMap f)).run
    }
  }
}
case class Return[A](a: A) extends IO[A]
case class Suspend[A](resume: () => A) extends IO[A]
case class FlatMap[A,B](fb:IO[B], f: B => IO[A]) extends IO[A]

object IO {
  def apply[A](a:A):IO[A] =
    Suspend(() => a)

  def printLine(inp:String):IO[Unit] =
    Suspend(() => println(inp))

  val random:IO[Int] =
    Suspend(() => Random.nextInt)
}