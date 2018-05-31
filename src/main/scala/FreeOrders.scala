import cats.free.Free
import cats.free.Free._

import cats.{Id, ~>}

object FreeOrders {

  type Symbol = String
  type Response = String

  sealed trait Orders[A]
  case class Buy(stock: Symbol, amount: Int) extends Orders[Response]
  case class Sell(stock: Symbol, amount: Int) extends Orders[Response]


  type OrdersF[A] = Free[Orders, A]

  def buy(stock: Symbol, amount: Int): OrdersF[Response] = liftF[Orders, Response](Buy(stock, amount))
  def sell(stock: Symbol, amount: Int): OrdersF[Response] = liftF[Orders, Response](Sell(stock, amount))

  def orderPrinter: Orders ~> Id =
    new (Orders ~> Id) {
      def apply[A](fa: Orders[A]): Id[A] = fa match {
        case Buy(stock, amount) =>
          println(s"Buying $amount of $stock")
          "ok": Id[Response]
        case Sell(stock, amount) =>
          println(s"Selling $amount of $stock")
          "ok": Id[Response]
      }
    }

  def smartTrade(): OrdersF[Response] = {
    for {
      _ <- buy("APPL", 50)
      _ <- buy("MSFT", 10)
      rsp <- sell("GOOG", 200)
    } yield rsp
  }

  def printOrders() = smartTrade().foldMap(orderPrinter)


  
}
