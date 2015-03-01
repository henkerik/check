package nl.henkerikvanderhoek.check

trait Showable[A] {
  def show(a:A):String
}

object Showable {
  def apply[A](implicit e:Showable[A]):Showable[A] = e

  implicit def ShowChar = new Showable[Char] {
    def show(a:Char) = a.toString
  }

  implicit def ShowBoolean = new Showable[Boolean] {
    def show(a:Boolean):String = a.toString
  }

  implicit def ShowInt = new Showable[Int] {
    def show(a:Int) = a.toString
  }

  implicit def ShowPair[A:Showable,B:Showable] = new Showable[(A,B)] {
    def show(pair:(A,B)):String =
      pair match { case (a,b) => "(" + a + "," + b + ")" }
  }

  implicit def ShowTriple[A:Showable,B:Showable,C:Showable] = new Showable[(A,B,C)] {
    def show(triple:(A,B,C)):String =
      triple match { case (a,b,c) => "(" + a + "," + b + "," + c + ")" }
  }

  implicit def ShowList[A:Showable] = new Showable[List[A]] {
    def show(xs:List[A]):String =
      "List(" + { xs.map { x => Showable[A].show(x) }.mkString(",") } + ")"
  }

  implicit def ShowSet[A:Showable] = new Showable[Set[A]] {
    def show(set:Set[A]):String = set.toString
  }
}