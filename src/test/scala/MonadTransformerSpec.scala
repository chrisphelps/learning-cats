import cats.data.EitherT
import cats.instances.future._
import cats.syntax.applicative._
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}

import scala.concurrent.duration._

class MonadTransformerSpec extends FlatSpec with Matchers {

  type ResponseStack[A] = Future[Either[String, A]]
  type Response[A] = EitherT[Future, String, A]

  def getPowerLevelStack(autobot: String): ResponseStack[Int] = {
    val level = powerLevels.get(autobot)
    val either = level.toRight(s"Comms error. $autobot unreachable.")
    Future.successful(either)
  }

  def getPowerLevel(autobot: String): Response[Int] = {
    powerLevels.get(autobot) match {
      case Some(level) => level.pure[Response]
      case None => EitherT.left(Future(s"Comms error: $autobot unreachable."))
    }
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      p1 <- getPowerLevel(ally1)
      p2 <- getPowerLevel(ally2)
    } yield p1 + p2 > 15


  def tacticalReport(ally1: String, ally2: String): String = {
    val response = canSpecialMove(ally1, ally2)
    val result = Await.result(response.value, 1.second)
    result match {
      case Left(err) => err
      case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) => s"$ally1 and $ally2 need a recharge."
    }
  }

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )


  "Autobots" should "find a powerlevel" in {
    val stack = getPowerLevelStack("Jazz")
    val result = Await.result(stack, 1.second)
    result should be(Right(6))
  }

  it should "return an error" in {
    val stack = getPowerLevelStack("Bogon")
    val result = Await.result(stack, 1.second)
    result shouldBe a[Left[_, _]]
  }

  it should "find a powerlevel with EitherT" in {
    val xform = getPowerLevel("Jazz")

    val result = Await.result(xform.isRight, 1.second)
    result should be(true)
  }

  it should "return an error with EitherT" in {
    val xform = getPowerLevel("Bogon")
    val result = Await.result(xform.isLeft, 1.second)
    result should be(true)
  }

  it should "permit special move when combined power is greater than 15" in {
    val xform = canSpecialMove("Hot Rod", "Bumblebee")
    val result = Await.result(xform.value, 1.second)
    result shouldBe a[Right[_,_]]
    result.right.get should be(true)
  }

  it should "disallow special move when combined power is less than 15" in {
    val xform = canSpecialMove("Jazz", "Bumblebee")
    val result = Await.result(xform.value, 1.second)
    result shouldBe a[Right[_,_]]
    result.right.get should be(false)
  }

  it should "give a message when it cannot get info for an ally" in {
    val xform = canSpecialMove("Bogon", "Bumblebee")
    val result = Await.result(xform.value, 1.second)
    result shouldBe a[Left[_,_]]
    //println(result.left.get)
  }

  it should "report tactically when allies cannot special move" in {
    val report = tacticalReport("Jazz", "Bumblebee")
    report should be("Jazz and Bumblebee need a recharge.")
  }

  it should "report tactically when allies can special move" in {
    val report = tacticalReport("Bumblebee", "Hot Rod")
    report should be("Bumblebee and Hot Rod are ready to roll out!")
  }

  it should "report tactically when comms fail" in {
    val report = tacticalReport("Jazz", "Ironhide")
    report should be("Comms error: Ironhide unreachable.")
  }

}
