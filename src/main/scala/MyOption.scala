import cats.Monad

import scala.annotation.tailrec

sealed trait MyOption[+A]
case object MyNone extends MyOption[Nothing]
final case class MySome[A](value: A) extends MyOption[A]

object MyOption {

  implicit val myOptionMonad = new Monad[MyOption] {
    def pure[A](value: A): MyOption[A] = MySome(value)

    def flatMap[A, B](opt: MyOption[A])(fn: A => MyOption[B]): MyOption[B] = opt match {
      case MyNone => MyNone
      case MySome(value) => fn(value)
    }

    //keep calling f until a right is returned
    // todo how to test directly? not sure exactly how to call this so that it matches regular flatMap
    @tailrec
    def tailRecM[A, B](a: A)(f: A => MyOption[Either[A, B]]): MyOption[B] = f(a) match {
      case MyNone => MyNone
      case MySome(Left(l)) => tailRecM(l)(f)
      case MySome(Right(r)) => MySome(r)
    }
  }

}
