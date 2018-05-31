import cats.data.Reader
import cats.syntax.applicative._
import org.scalatest.{FlatSpec, Matchers}

class ReaderSpec extends FlatSpec with Matchers {

  case class Db(
               usernames: Map[Int, String],
               passwords: Map[String, String]
               )

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

//  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
//    for {
//      username <- findUsername(userId)
//      valid <- username.map{ un => checkPassword(un, password) }.getOrElse{ false.pure[DbReader] }
//    } yield valid

  val db = Db(
    Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo",
      5 -> "tester",
      6 -> ""
    ),
    Map(
      "dade" -> "zerocool",
      "kate" -> "acidburn",
      "margo" -> "secret",
      "tester" -> "",
      "" -> ""
    )
  )


  "Reader" should "find usernames" in {
    findUsername(1).run(db) should be(Some("dade"))
    findUsername(4).run(db) should be(None)
  }

  it should "check passwords" in {
    checkPassword("dade", "zerocool").run(db) should be(true)
    checkPassword("dade", "waycool").run(db) should be(false)
    checkPassword("foobar", "badpass").run(db) should be(false)
  }

//  it should "do the final check" in {
//    checkLogin(1, "zerocool").run(db) should be(true)
//    checkLogin(4, "davinci").run(db) should be(false)
//  }
//
//  it should "not be goofed up by empty usernames or passwords" in {
//    checkLogin(4, "").run(db) should be(false)
//    checkLogin(5, "").run(db) should be(true)
//    checkLogin(6, "").run(db) should be(true)
//  }

}
