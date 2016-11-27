import cats.data.State
import cats.syntax.applicative._
import org.scalatest.{FlatSpec, Matchers}

class StateSpec extends FlatSpec with Matchers {

  type CalcState[A] = State[List[Int], A]

  def operator(stack: List[Int], f: (Int, Int) => Int): (List[Int], Int) =
    stack match {
      case a :: b :: tail =>
        val res = f(b, a)
        (res :: tail, res)
      case _ => ???
    }

  def operand(stack: List[Int], num: String): (List[Int], Int) = {
    val res = num.toInt
    (res :: stack, res)
  }


  def evalOne(sym: String): CalcState[Int] =
    State[List[Int], Int] { oldStack =>
      sym match {
        case "+" => operator(oldStack, _ + _)
        case "-" => operator(oldStack, _ - _)
        case "*" => operator(oldStack, _ * _)
        case "/" => operator(oldStack, _ / _)
        case _ => operand(oldStack, sym)
      }
    }

  // book abstracts the state creation down into the operator. maybe tidies the signatures a bit

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState])((s, in) => s.flatMap(_ => evalOne(in)))


  "State" should "add a number to an empty stack" in {
    evalOne("1").runA(Nil).value should be(1)
  }

  it should "add to a non-empty stack" in {
    val (st, res) = evalOne("2").run(List(1)).value
    res should be(2)
    st should be(List(2, 1))
  }

  it should "add two stack elements" in {
    val (st, res) = evalOne("+").run(List(1, 2)).value
    st should be(List(3))
    res should be(3)
  }

  it should "subtrack stack elements" in {
    val (st, res) = evalOne("-").run(List(1, 2)).value
    st should be(List(1))
    res should be(1)
  }

  it should "multiply stack elements" in {
    val (st, res) = evalOne("*").run(List(1, 2)).value
    st should be(List(2))
    res should be(2)
  }

  it should "divide stack elements" in {
    val (st, res) = evalOne("/").run(List(1, 2)).value
    st should be(List(2))
    res should be(2)
  }

  it should "compose a calc program" in {
    val program = for {
      _ <- evalOne("1")
      _ <- evalOne("2")
      ans <- evalOne("+")
    } yield ans
    program.runA(Nil).value should be(3)
  }

  it should "evalAll to compose a program" in {
    val program = evalAll(List("1", "2", "+", "3", "*"))
    program.runA(Nil).value should be(9)
  }

  it should "compose evalAll programs" in {
    val program = for {
      _ <- evalAll(List("1", "2", "+"))
      _ <- evalAll(List("3", "4", "+"))
      ans <- evalOne("*")
    } yield ans
    program.runA(Nil).value should be(21)
  }
}
