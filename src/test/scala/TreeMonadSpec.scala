import cats.Monad
import org.scalatest.{FlatSpec, Matchers}
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.flatMap._


class TreeMonadSpec extends FlatSpec with Matchers {

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A] = Leaf(value)

  implicit val treeMonad = new Monad[Tree] {
    def pure[A](value: A): Tree[A] = Leaf(value)

    def flatMap[A, B](tree: Tree[A])(fn: A => Tree[B]): Tree[B] = tree match {
      case Branch(left, right) => Branch(flatMap(left)(fn), flatMap(right)(fn))
      case Leaf(v) => fn(v)
    }

    // todo work through this more. I understand it but can't entirely write it (blog?)
    def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = f(a) match {
      case Branch(l,r) =>
        Branch(flatMap(l) {
          case Left(l) => tailRecM(l)(f)
          case Right(r) => pure(r)
        }, flatMap(r) {
          case Left(l) => tailRecM(l)(f)
          case Right(r) => pure(r)
        })

      case Leaf(Left(l)) => tailRecM(l)(f)
      case Leaf(Right(r)) => Leaf(r)
    }
  }

  "Tree Monad" should "implement pure" in {
    treeMonad.pure(1) should be(leaf(1))
  }

  it should "support implicit resolution" in {
    Monad[Tree].pure(1) should be(leaf(1))
  }

  it should "support applicative pure" in {
    1.pure[Tree] should be(Leaf(1))
  }

  it should "implement flatMap for a leaf" in {
    val tree = 2.pure[Tree]
    val res = Monad[Tree].flatMap(tree)(x => (x + 1).pure[Tree])
    res should be(Leaf(3))
  }

  it should "implement flatMap for a branch" in {
    val tree = Branch(Leaf(2), Leaf(3))
    val res = Monad[Tree].flatMap(tree)(x => (x * 2).pure[Tree])
    res should be(Branch(Leaf(4), Leaf(6)))
  }

  it should "do functor for free" in {
    val tree = Branch(Leaf(2), Leaf(3))
    val res = Monad[Tree].map(tree)(x => x * 2)
    res should be (Branch(Leaf(4), Leaf(6)))
  }

  it should "do flatMap and map" in {
    val tree = branch(leaf(100), leaf(200))
    val res = tree.flatMap(x => branch(leaf(x - 1), leaf(x + 1)))

    res should be(Branch(Branch(Leaf(99), Leaf(101)), Branch(Leaf(199), Leaf(201))))
  }

  it should "support for-comprehension" in {
    val res = for {
      a <- branch(leaf(100), leaf(200))
      b <- branch(leaf(a - 10), leaf(a + 10))
      c <- branch(leaf(b - 1), leaf(b + 1))
    } yield c

    res should be (Branch(
                    Branch(
                      Branch(
                        Leaf(89),
                        Leaf(91)),
                      Branch(
                        Leaf(109),
                        Leaf(111))),
                    Branch(
                      Branch(
                        Leaf(189),
                        Leaf(191)),
                      Branch(
                        Leaf(209),
                        Leaf(211)))))
  }

}
