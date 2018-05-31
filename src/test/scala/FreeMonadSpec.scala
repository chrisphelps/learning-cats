import org.scalatest.{FlatSpec, Matchers}

class FreeMonadSpec extends FlatSpec with Matchers{

  "FreeOrders" should "print orders" in {
    FreeOrders.printOrders()
  }

}
