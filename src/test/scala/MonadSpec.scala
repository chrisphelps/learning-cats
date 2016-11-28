import cats.Eval
import org.scalatest.{FlatSpec, Matchers}

import scala.language.higherKinds

class MonadSpec extends FlatSpec with Matchers {

  trait Monad[F[_]] {
    def pure[A](a: A): F[A]

    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

    def map[A, B](value: F[A])(func: A => B): F[B] = flatMap(value)(a => pure(func(a)))
  }

  case class Foo[A](value: A)

  implicit val fooMonad = new Monad[Foo] {
    def pure[A](a: A): Foo[A] = Foo(a)
    def flatMap[A, B](value: Foo[A])(func: A => Foo[B]): Foo[B] = func(value.value)
  }

  "Monad" should "provide a map" in {
    val om = fooMonad.pure(5)
    fooMonad.map(om)(_ + 1) should be(Foo(6))
  }


  type Id[A] = A

  implicit val idMonad = new Monad[Id] {
    def pure[A](a: A): Id[A] = a

    def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = func(value)

    override def map[A, B](value: Id[A])(func: A => B): Id[B] = func(value)
  }

  "Id" should "support pure" in {
    val a = idMonad.pure(3)
    a should be(3)
  }

  it should "be correct for various types" in {
    idMonad.pure("energy") should be ("energy")
  }

  it should "support flatMap" in {
    val a = idMonad.pure(3)
    idMonad.flatMap(a)(_ + 1) should be(4)
  }

  it should "support map" in {
    val a = idMonad.pure(3)
    idMonad.map(a)(_ + 1) should be(4)
  }


  def foldRight[A, B](as: List[A], acc: B)(fn: (A,B) => B): B =
    as match {
      case head :: tail => fn(head, foldRight(tail, acc)(fn))
      case Nil => acc
    }

  def safeFoldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] = {
    as match {
      case head :: tail => Eval.defer(safeFoldRight(tail, acc)(fn)).map( b => fn(head, b))
      case Nil => Eval.now(acc)
    }
  }

  "Eval" should "prevent blowing stack" in {
    val res = safeFoldRight((1 to 10000).toList, 0: BigInt)(_ + _)
    res.value
  }

  it should "give the same result for regular and safe foldRight" in {
    val as = (1 to 5000).toList
    val r = foldRight(as, 0: BigInt)(_ + _)
    val s = safeFoldRight(as, 0: BigInt)(_ + _).value
    r should be(s)
  }

}
