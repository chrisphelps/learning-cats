import cats.data.Validated
import org.scalatest.{FlatSpec, Matchers}
import cats.syntax.either._
import cats.syntax.cartesian._
import cats.instances.list._

import scala.util.Try

class ValidatedSpec extends FlatSpec with Matchers {

  case class User(name: String, age: Int)

  def getValue(form: Map[String, String])(field: String): Either[List[String], String] = {
    form.get(field).toRight(List(s"Field $field not found"))
  }

  def parseInt(input: String): Either[List[String], Int] = {
    Try(input.toInt).toEither.leftMap(t => List("Error converting to int"))
  }

  def nonBlank(input: String): Either[List[String], String] = {
    input match {
      case "" => Left(List("String field is blank"))
      case _ => Right(input)
    }
  }

  def nonNegative(input: Int): Either[List[String], Int] = {
    if (input >= 0) Right(input) else Left(List("Integer field is negative"))
  }

  def readName(form: Map[String, String]): Either[List[String], String] = {
    getValue(form)("name").flatMap(nonBlank)
  }

  def readAge(form: Map[String, String]): Either[List[String], Int] = {
    getValue(form)("age").flatMap(parseInt).flatMap(nonNegative)
  }

  def readUser(form: Map[String, String]): Validated[List[String], User] = {
    (
      readName(form).toValidated |@|
      readAge(form).toValidated
    ).map(User.apply)
  }

  "GetValue" should "get a value from a map" in {
    val form = Map("name" -> "Chris", "state" -> "CO")

    val result = getValue(form)("name")

    result should be(Right("Chris"))
  }

  it should "return an error message if the key isn't there" in {
    val form = Map("name" -> "Chris", "state" -> "CO")

    val result = getValue(form)("bogus")

    result should be(Left(List("Field bogus not found")))
  }

  "ParseInt" should "parse an int" in {
    parseInt("1234") should be(Right(1234))
  }

  it should "return error message if failing" in {
    parseInt("bogus") should be(Left(List("Error converting to int")))
  }

  "NonBlank" should "get a value if nonblank" in {
    nonBlank("foo") should be (Right("foo"))
  }

  it should "return error message if blank" in {
    nonBlank("") should be(Left(List("String field is blank")))
  }

  "NonNegative" should "return the value if positive" in {
    nonNegative(5) should be(Right(5))
  }

  it should "return error message if negative" in {
    nonNegative(-5) should be (Left(List("Integer field is negative")))
  }

  "ReadName" should "return a name if non-blank" in {
    val form = Map("name" -> "Chris", "state" -> "CO")

    readName(form) should be(Right("Chris"))
  }

  it should "return error message when key does not exist" in {
    val form = Map("state" -> "CO")

    readName(form) shouldBe a [Left[_, _]]
  }

  it should "return error message when key is empty" in {
    val form = Map("name" -> "")

    readName(form) shouldBe a [Left[_,_]]
  }

  "ReadAge" should "return an age if numeric and positive" in {
    val form = Map("age" -> "42")

    readAge(form) should be(Right(42))
  }

  it should "return error message when value is non-numeric" in {
    val form = Map("age" -> "forty-two")

    readAge(form) shouldBe a [Left[_, _]]
  }

  it should "return error message when value is negative" in {
    val form = Map("age" -> "-5")

    readAge(form) shouldBe a [Left[_, _]]
  }

  "ReadUser" should "read a user when all the stuff is right" in {
    val form = Map("name" -> "Chris", "age" -> "42")

    readUser(form) should be(Validated.Valid(User("Chris", 42)))
  }

  it should "collect errors when stuff is wrong" in {
    val form = Map("name" -> "", "age" -> "-5")

    val result = readUser(form)

    result shouldBe a [Validated.Invalid[_]]
    result.leftMap(l => l.size should be(2))
  }
}
