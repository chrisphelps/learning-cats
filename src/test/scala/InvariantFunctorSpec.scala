import org.scalatest.{FlatSpec, Matchers}

class InvariantFunctorSpec extends FlatSpec with Matchers {

  trait Codec[A] {
    def encode(value: A): String
    def decode(value: String): Option[A]

    def imap[B](dec: A => B, enc: B => A): Codec[B] = {
      val self = this
      new Codec[B] {
        def encode(value: B): String = self.encode(enc(value))
        def decode(value: String): Option[B] = self.decode(value).map(dec)
      }
    }
  }

  def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): Option[A] = c.decode(value)

  implicit val intCodec = new Codec[Int] {
    def encode(value: Int): String = value.toString
    def decode(value: String): Option[Int] = {
      try {
        Some(value.toInt)
      }
      catch {
        case _: Throwable => None
      }
    }
  }


  "Codec" should "encode ints" in {
    encode(123) should be("123")
  }

  it should "decode ints" in {
    decode[Int]("123") should be(Some(123))
  }



  implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] =
    c.imap(a => Box(a), b => b.value)


  it should "encode Boxes" in {
    encode(Box(123)) should be("123")
  }

  it should "decode Boxes" in {
    decode[Box[Int]]("123") should be(Some(Box(123)))
  }

}
