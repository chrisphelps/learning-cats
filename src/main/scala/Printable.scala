import cats.Show
import cats.Eq

trait Printable[A] {
  def format(item: A): String
}

object PrintDefaults {
  implicit val StringPrinter = new Printable[String] {
    def format(item: String) = item
  }

  implicit val IntPrinter = new Printable[Int] {
    def format(item: Int) = item.toString
  }
}

object Print {
  def format[A](item: A)(implicit printer: Printable[A]): String = printer.format(item)

  def print[A](item: A)(implicit printer: Printable[A]): Unit = println(format(item))
}

final case class Cat(name: String, age: Int, color: String)

object CatPrint {
  implicit val CatPrinter = new Printable[Cat] {
    def format(item: Cat) = s"${item.name} is a ${item.age} year old ${item.color} cat."
  }
}

object PrintSyntax {
  implicit class PrintOps[A](value: A) {
    def format(implicit printer: Printable[A]): String = printer.format(value)

    def print(implicit printer: Printable[A]): Unit = println(format)
  }
}


object Main extends App {
  import CatPrint._
  val genji = Cat("Genji", 5, "cow")
  val snoop = Cat("Snoop", 11, "grey")
  val harley = Cat("Harley", 14, "cinnamon")

  println("Output from interface object")
  Print.print(genji)
  Print.print(snoop)
  Print.print(harley)

  import PrintSyntax._
  println("Output from syntax")
  genji.print
  snoop.print
  harley.print

  implicit val catShow = Show.show[Cat](cat => s"${cat.name} is a ${cat.age} year old ${cat.color} cat.")
  println("Output from Show")
  println(catShow.show(genji))
  println(catShow.show(snoop))
  println(catShow.show(harley))

  import cats.syntax.show._
  println("Output from Show syntax")
  println(genji.show)
  println(snoop.show)
  println(harley.show)

  implicit val catEq = Eq.instance[Cat]{(cat1, cat2) => cat1.name == cat2.name && cat1.color == cat2.color}
  val newgenji = Cat("Genji", 8, "cow")

  println("Equivalence")
  println(s"Genji and new-Genji: ${Eq.eqv(genji, newgenji)}")
  println(s"Genji and Harley: ${Eq.eqv(genji, harley)}")

  import cats.syntax.eq._
  println(s"Genji and new-Genji: ${genji === newgenji}")
  println(s"Genji and Harley: ${genji === harley}")

  val optGenji: Option[Cat] = Some(genji)
  val optNone: Option[Cat] = None
  val optNewGenji: Option[Cat] = Some(newgenji)

  import cats.std.option._
  println(s"Optional Genji and None: ${optGenji === optNone}")
  println(s"Optional Genji and Optional new-Genji: ${optGenji === optNewGenji}")

}
