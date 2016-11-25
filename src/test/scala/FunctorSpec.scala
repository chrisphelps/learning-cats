
import cats.Functor
import org.scalatest.{FlatSpec, Matchers}

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

class FunctorSpec extends FlatSpec with Matchers {

  implicit val treeFunctor = new Functor[Tree] {
    def map[A, B](value: Tree[A])(f: A => B): Tree[B] = {
      value match {
        case Leaf(a) => Leaf(f(a))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      }
    }
  }

  "Tree Functor" should "map a leaf" in {
    val leaf = Leaf(5)
    val result = Functor[Tree].map(leaf)(_ + 1)
    result should be(Leaf(6))
  }

  it should "map a branch" in {
    val branch = Branch(Leaf(1), Leaf(2))
    val result = Functor[Tree].map(branch)(_ + 1)
    result should be(Branch(Leaf(2), Leaf(3)))
  }

  it should "map for multiple types" in {
    val branch = Branch(Leaf("foo"), Leaf("longer"))
    val result = Functor[Tree].map(branch)(_.length)
    result should be(Branch(Leaf(3), Leaf(6)))
  }
}
