import java.util

import TreeMonad.{Branch, Leaf, Tree, TreeMonad}
import cats.Monad
import cats.kernel.Eq
import cats.laws.discipline.MonadTests
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}
import org.typelevel.discipline.scalatest.Discipline

import scala.annotation.tailrec

class TreeMonadSpec extends FunSuite with Matchers with PropertyChecks with Discipline with TreeGen {

  implicit override val generatorDrivenConfig: PropertyCheckConfig =
    PropertyCheckConfig(minSize = 0, maxSize = 1E2.toInt, minSuccessful = 10)

  import cats.instances.int._
  import cats.instances.tuple._

  implicit val tm: TreeMonad = new TreeMonad

  val chars: List[Char] = "abcdefghijklmnopqrstuwxyzABCDEFGHIJKLMNOPQRSTUWXYZ01234567890".toList

  test("simple flatMap case #1") {
    val tree = Leaf(1)
    def f(a: Int): Tree[Int] = Leaf(a+1)
    tm.flatMap(tree)(f) shouldBe Leaf(2)
  }

  test("simple flatMap case #2") {
    val tree = Branch(Leaf(1),Leaf(2))
    def f(a: Int): Tree[Int] = Leaf(a+1)
    tm.flatMap(tree)(f) shouldBe Branch(Leaf(2),Leaf(3))
  }

  test("simple flatMap case #3") {
    val tree = Leaf(1)
    def f(a: Int): Tree[Int] = Branch(Leaf(a+1),Leaf(a+2))
    tm.flatMap(tree)(f) shouldBe Branch(Leaf(2),Leaf(3))
  }

  test("simple flatMap case #4") {
    val tree = Branch(Leaf(1),Leaf(2))
    def f(a: Int): Tree[Int] = Branch(Leaf(a+1),Leaf(a+2))
    tm.flatMap(tree)(f) shouldBe Branch(Branch(Leaf(2),Leaf(3)),Branch(Leaf(3),Leaf(4)))
  }

  test("simple flatMap case #5") {
    val tree = Branch(Branch(Leaf(2),Leaf(3)),Branch(Leaf(5),Leaf(8)))
    def f(a: Int): Tree[String] = Leaf(s"$a")
    tm.flatMap(tree)(f) shouldBe Branch(Branch(Leaf("2"),Leaf("3")),Branch(Leaf("5"),Leaf("8")))
  }

  test("simple flatMap case #6") {
    val t = Gen.listOfN(1000000,Gen.alphaNumChar).map(symmetricTree).sample.get
    println(s"symmetric tree depth = ${t.depth}")
    def f(a: Char): Tree[String] = Leaf(s"$a")
    tm.flatMap(t)(f).depth shouldBe t.depth
  }

  test("simple flatMap case #7") {
    val t = Gen.listOfN(1000000,Gen.alphaNumChar).map(x => asymmetricTree(x.tail, Leaf(x.head))).sample.get
    println(s"asymmetric tree depth = ${t.depth}")
    def f(a: Char): Tree[String] = Leaf(s"$a")
    tm.flatMap(t)(f).depth shouldBe t.depth
  }

  test("simple tailRecM case #1") {
    def f(a: Int): Tree[Either[Int,Int]] = {
      Branch(Branch(Leaf(Right(a)), Leaf(Right(a+1))), Leaf(Right(a+2)))
    }
    tm.tailRecM(3)(f) shouldBe Branch(Branch(Leaf(3), Leaf(4)), Leaf(5))
  }

  test("simple tailRecM case #2") {
    def f(a: Int): Tree[Either[Int,Int]] = {
      Branch(Branch(Leaf(Right(a)), Leaf(Right(a+1))), Branch(Leaf(Right(a+3)), Leaf(Right(a+2))))
    }
    tm.tailRecM(3)(f) shouldBe Branch(Branch(Leaf(3), Leaf(4)), Branch(Leaf(6), Leaf(5)))
  }

  test("simple tailRecM case #3") {
    def f(a: Int): Tree[Either[Int,Int]] = {
      if(a <= 5) Branch(Leaf(Left(a+1)), Leaf(Right(a)))
      else Branch(Leaf(Right(a)), Leaf(Right(a+1)))
    }
    tm.tailRecM(3)(f) shouldBe Branch(Branch(Branch(Branch(Leaf(6), Leaf(7)), Leaf(5)), Leaf(4)), Leaf(3))
  }

  test("simple tailRecM case #4") {
    def f(a: Int): Tree[Either[Int,Int]] = {
      if(a <= 5) Branch(Leaf(Left(a+1)), Leaf(Left(a+2)))
      else Branch(Leaf(Right(a)), Leaf(Right(a+1)))
    }
    tm.tailRecM(3)(f) shouldBe Branch(
      Branch(
        Branch(
          Branch(Leaf(6), Leaf(7)),
          Branch(Leaf(7), Leaf(8))
        ),
        Branch(Leaf(6), Leaf(7))
      ),
      Branch(
        Branch(Leaf(6), Leaf(7)),
        Branch(Leaf(7), Leaf(8))
      )
    )
  }

  test("flatMap stack safety case #1") {
    forAll(symmetricTreeGen(Gen.choose(0,99))) { (tree: Tree[Int]) =>
      val tree1 = Monad[Tree].flatMap(tree)(x => Leaf(100 - x))
      tree1.depth shouldBe tree.depth
    }
  }

  test("flatMap stack safety case #2") {
    forAll(symmetricTreeGen(Gen.choose(0,99))) { (tree: Tree[Int]) =>
      val tree1 = Monad[Tree].flatMap(tree)(x => Branch(Leaf(x),Leaf(x)))
      tree1.depth shouldBe (tree.depth + 1)
    }
  }

  test("stack overflow") {
    def b(a: Int): Tree[Either[Int,Int]] = {
      if(a<1000000) Branch(Leaf(Left(a+1)), Leaf(Right(a)))
      else Leaf(Right(a))
    }
    val tm1 = tm.tailRecM(1)(a => Branch(b(a), b(a)))
    println(tm1.depth)
  }

  checkAll("TreeMonad", MonadTests[Tree].monad[Int, Int, Int])


}

trait TreeGen {

  def symmetricTree[A](list: List[A]): Tree[A] = {
    val s = new util.Stack[Tree[A]]
    s.push(Leaf(list.head))

    @tailrec
    def build(list: List[A]): Tree[A] = list match {
      case Nil =>
        merge(s.pop(), force = true)
      case x :: xs =>
        val leaf = Leaf(x)
        s.pop() match {
          case l @ Leaf(_) =>
            s.push(Branch(l, leaf))
          case b @ Branch(_,_) =>
            merge(b)
            s.push(leaf)
        }
        build(xs)
    }

    @tailrec
    def merge(tree: Tree[A], force: Boolean = false): Tree[A] = {
      if(!s.isEmpty) {
        val prev = s.peek()
        if(force || prev.depth==tree.depth) {
          merge(Branch(s.pop(),tree), force)
        } else {
          s.push(tree)
        }
      } else s.push(tree)
    }

    build(list.tail)
  }

  def symmetricTreeGen[A](gen: Gen[A]): Gen[Tree[A]] = Gen.nonEmptyListOf(gen)
    .map(symmetricTree)

  @tailrec
  final def asymmetricTree[A](list: List[A], left: Tree[A], right: Option[Tree[A]] = None): Tree[A] =
    list match {
      case Nil => left
      case x :: xs => right match {
        case None => asymmetricTree(xs, left, Some(Leaf(x)))
        case Some(l @ Leaf(_)) => asymmetricTree(xs, left, Some(Branch(l, Leaf(x))))
        case Some(r @ Branch(_,_)) => asymmetricTree(xs, Branch(left, r), Some(Leaf(x)))
      }
    }

  def asymmetricTreeGen[A](gen: Gen[A]): Gen[Tree[A]] = Gen.nonEmptyListOf(gen)
    .map(list => asymmetricTree(list.tail, Leaf(list.head)))

  implicit def arbitraryTree[A](implicit a: Arbitrary[A]): Arbitrary[Tree[A]] =
    Arbitrary(symmetricTreeGen(a.arbitrary))

  implicit def eqTree[A](implicit t: Eq[A]): Eq[Tree[A]] = new Eq[Tree[A]] {
    def eqv(x: Tree[A], y: Tree[A]): Boolean = {
      x == y
    }
  }

}
