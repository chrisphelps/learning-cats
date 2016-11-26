
import cats._
import org.scalatest.{FlatSpec, Matchers}

import cats.instances.string._
import cats.instances.int._
import cats.instances.option._
import cats.syntax.semigroup._

class MonoidSpec extends FlatSpec with Matchers {

  def add[T](items: List[T])(implicit m: Monoid[T]): T = {
    m.combineAll(items)
  }

  "Monoids" should "combine without a saved instance" in {
    val combined = Monoid[String].combine("Hi ", "there")
    combined should be ("Hi there")
  }

  it should "combine via an instance" in {
    val instance = Monoid[String]
    instance.combine("foo", "bar") should be ("foobar")
  }

  it should "combine strings with syntax" in {
    val scombined = "Some" |+| "Combination" |+| "Of" |+| "Stuff"
    scombined should be ("SomeCombinationOfStuff")
  }

  it should "combine ints with syntax" in {
    val icombined = 1 |+| 6
    icombined should be (7)
  }

  "Superadder" should "add a list of ints" in {
    val list = (1 to 10).toList

    add(list) should be (55)
  }

  it should "add a list of optional ints" in {
    val list: List[Option[Int]] = (1 to 10).map(Some.apply).toList

    add(list) should be (Some(55))
  }

  it should "add list of optionals with some missing" in {
    val sl = (1 to 10).map(Some.apply).toList
    val nl = (1 to 10).map(i => None).toList
    val list: List[Option[Int]] = (sl zip nl).flatMap(t => List(t._1, t._2))

    list.size should be (20)
    add(list) should be (Some(55))
  }

  it should "add orders" in {
    case class Order(totalCost: Double, quantity: Double)

    val o1 = Order(1.5, 2)
    val o2 = Order(15.2, 7)

    implicit val OrderMonoid = new Monoid[Order] {
      def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
      val empty = Order(0,0)
    }

    add(List(o1, o2)) should be (Order(16.7, 9))
  }
}
