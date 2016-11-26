import cats.data.Writer
import cats.syntax.writer._
import org.scalatest.{FlatSpec, Matchers}
import cats.instances.vector._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class WriterSpec extends FlatSpec with Matchers {

  def slowly[A](body: => A): A =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    //println(s"fact $n $ans")
    ans
  }

  "Writer" should "run factorial" in {
    factorial(5)
  }

  it should "run factorial in parallel" in {
    Await.result(Future.sequence(Vector(
      Future(factorial(3)),
      Future(factorial(3))
    )), Duration.Inf)
  }


  def writerFactorial(n: Int): Writer[Vector[String], Int] = {
    if (n == 0)
      1.writer(Vector(s"fact 0 1"))
    else
      writerFactorial(n - 1).flatMap(r => (n * r).writer(Vector(s"fact $n ${n * r}")))
  }

  it should "run writer factorial" in {
    val w = writerFactorial(5)

    val (log, result) = w.run
    //println(s"log: $log")
    //println(s"result: $result")
  }

  it should "run writer factorial in parallel" in {
    val rs = Await.result(Future.sequence(Vector(
      Future(writerFactorial(3)),
      Future(writerFactorial(3))
    )), Duration.Inf)
    rs.size should be(2)
    val w1 = rs(0)
    val w2 = rs(1)

    val (log1, _) = w1.run
    val (log2, _) = w2.run

    log1 should be(log2)
  }
}
