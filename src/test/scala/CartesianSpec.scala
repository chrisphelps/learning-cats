import org.scalatest.{FlatSpec, Matchers}

import scala.language.higherKinds
import cats.Monad

import cats.instances.option._
import cats.syntax.functor._
import cats.syntax.flatMap._

class CartesianSpec extends FlatSpec with Matchers {

  def productDesugared[M[_]: Monad, A, B](fa: M[A], fb: M[B]): M[(A, B)] =
    fa.flatMap (a => fb.map(b => (a, b)))

  def product[M[_]: Monad, A, B](fa: M[A], fb: M[B]): M[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)


  "Product" should "implement product semantics for present option" in {

    val fa: Option[Int] = Some(5)
    val fb: Option[Boolean] = Some(true)

    product(fa, fb) should be(Some(5, true))
  }

  it should "implement product for missing options" in {
    val fa: Option[Int] = None
    val fb: Option[Boolean] = Some(true)

    product(fa, fb) should be(None)
  }

  it should "implement product for missing options the other way" in {
    val fa: Option[Int] = Some(5)
    val fb: Option[Boolean] = None

    product(fa, fb) should be(None)
  }


}
