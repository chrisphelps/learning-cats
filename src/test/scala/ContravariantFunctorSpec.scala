import org.scalatest.{FlatSpec, Matchers}

final case class Box[A](value: A)

class ContravariantFunctorSpec extends FlatSpec with Matchers {

  trait Printable[A] {
    def format(value: A): String

    def contramap[B](func: B => A): Printable[B] = {
      val self = this
      new Printable[B] {
        def format(value: B): String = {
          self.format(func(value))
        }
      }
    }
  }

  def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)

  implicit val intPrinter = new Printable[Int] {
    def format(value: Int): String = value.toString
  }

  implicit val stringPrintable = new Printable[String] {
    def format(value: String): String = "\"" + value + "\""
  }

  implicit val booleanPrintable = new Printable[Boolean] {
    def format(value: Boolean): String = if (value) "yes" else "no"
  }


  "Printable contramap" should "build a printable" in {
    implicit val stringPrinter = intPrinter.contramap[String]((s: String) => s.length)
    stringPrinter.format("foobar") should be("6")
  }

  it should "do the implicit thing for Box[String]" in {
    implicit val boxPrinter = stringPrintable.contramap[Box[String]](b => b.value)
    format(Box("hello world")) should be("\"hello world\"")
  }

  it should "do the implicit thing for Box[Boolean]" in {
    implicit val boxPrinter = booleanPrintable.contramap[Box[Boolean]](b => b.value)
    format(Box(true)) should be("yes")
  }


  // todo don't quite understand why implicit params works here but not implicitly[]
  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] = p.contramap[Box[A]](_.value)

  it should "figure out the types implicitly" in {
    format(Box("hello world")) should be("\"hello world\"")
    format(Box(true)) should be("yes")
    format(Box(5)) should be("5")
  }
}
