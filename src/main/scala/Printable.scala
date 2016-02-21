
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
}
