
import cats.Monoid
import cats.std.string._
import cats.std.int._
import cats.syntax.semigroup._


object Monoids extends App {


  val combined = Monoid[String].combine("Hi ", "there")
  println(s"Combined: $combined")

  val instance = Monoid[String]
  println(instance.combine("foo", "bar"))


  val scombined = "Some" |+| "Combination" |+| "Of" |+| "Stuff"
  println(scombined)

  val icombined = 1 |+| 6
  println(icombined)
}
