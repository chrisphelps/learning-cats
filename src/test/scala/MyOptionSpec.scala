import MyOption._
import cats.instances.int._
import cats.instances.tuple._
import cats.kernel.Eq
import cats.laws.discipline._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{FunSuite, Matchers}
import org.typelevel.discipline.scalatest.Discipline


class MyOptionSpec extends FunSuite with Matchers with Discipline {

  implicit def arbMyOption[T](implicit a: Arbitrary[T]): Arbitrary[MyOption[T]] =
    Arbitrary(
      Gen.oneOf(
        arbitrary[T].map(v => MySome(v)),
        Gen.const(MyNone)
      )
    )


  implicit def eqMyOption[T](implicit t: Eq[T]): Eq[MyOption[T]] = new Eq[MyOption[T]] {
      def eqv(x: MyOption[T], y: MyOption[T]): Boolean = x match {
        case MyNone => y match {
          case MyNone => true
          case _ => false
        }
        case MySome(a) => y match {
          case MySome(b) if a == b => true
          case _ => false
        }
      }
    }


  checkAll("MyOption[Int]", MonadTests[MyOption].monad[Int, Int, Int])

}
