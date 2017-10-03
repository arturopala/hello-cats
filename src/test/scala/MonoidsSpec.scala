import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class MonoidsSpec extends FlatSpec with Matchers with PropertyChecks {

  "Boolean" should "have an AND monoid" in {
    implicit val m =  BooleanMonoids.And
    Monoid[Boolean].combine(false,m.empty) shouldBe false
    Monoid[Boolean].combine(true,m.empty) shouldBe true
    Monoid[Boolean].combine(true,true) shouldBe true
    Monoid[Boolean].combine(true,false) shouldBe false
    Monoid[Boolean].combine(false,true) shouldBe false
    Monoid[Boolean].combine(false,false) shouldBe false
  }

  it should "have an OR monoid" in {
    implicit val m =  BooleanMonoids.Or
    Monoid[Boolean].combine(false,m.empty) shouldBe false
    Monoid[Boolean].combine(true,m.empty) shouldBe true
    Monoid[Boolean].combine(true,true) shouldBe true
    Monoid[Boolean].combine(true,false) shouldBe true
    Monoid[Boolean].combine(false,true) shouldBe true
    Monoid[Boolean].combine(false,false) shouldBe false
  }

  it should "have a XOR monoid" in {
    implicit val m =  BooleanMonoids.Xor
    Monoid[Boolean].combine(false,m.empty) shouldBe false
    Monoid[Boolean].combine(true,m.empty) shouldBe true
    Monoid[Boolean].combine(true,true) shouldBe false
    Monoid[Boolean].combine(true,false) shouldBe true
    Monoid[Boolean].combine(false,true) shouldBe true
    Monoid[Boolean].combine(false,false) shouldBe false
  }

  it should "not have a NAND monoid" in {
    implicit val m =  BooleanMonoids.Nand
    an[Exception] should be thrownBy {
      Monoid[Boolean].combine(false,m.empty) shouldBe false
      Monoid[Boolean].combine(true,m.empty) shouldBe true
      Monoid[Boolean].combine(true, true) shouldBe false
      Monoid[Boolean].combine(true, false) shouldBe true
      Monoid[Boolean].combine(false, true) shouldBe true
      Monoid[Boolean].combine(false, false) shouldBe true
    }
  }

  it should "not have a NOR monoid" in {
    implicit val m =  BooleanMonoids.Nor
    an[Exception] should be thrownBy {
      Monoid[Boolean].combine(false,m.empty) shouldBe false
      Monoid[Boolean].combine(true,m.empty) shouldBe true
      Monoid[Boolean].combine(true, true) shouldBe false
      Monoid[Boolean].combine(true, false) shouldBe false
      Monoid[Boolean].combine(false, true) shouldBe false
      Monoid[Boolean].combine(false, false) shouldBe true
    }
  }

  "Set" should "have an UNION monoid" in {
    implicit val m = SetMonoids.union[Int]
    Monoid[Set[Int]].combine(m.empty,m.empty) shouldBe m.empty
    Monoid[Set[Int]].combine(Set(1,2),m.empty) shouldBe Set(2,1)
    Monoid[Set[Int]].combine(m.empty, Set(2,1)) shouldBe Set(1,2)
    Monoid[Set[Int]].combine(Set(4,3,5), Set(2,1)) shouldBe Set(1,2,4,3,5)
  }

  it should "not have an INTERSECTION monoid" in {
    implicit val m = SetMonoids.intersection[Int]
    an[Exception] should be thrownBy {
      Monoid[Set[Int]].combine(m.empty, m.empty) shouldBe m.empty
      Monoid[Set[Int]].combine(Set(1, 2), m.empty) shouldBe Set(1, 2)
      Monoid[Set[Int]].combine(m.empty, Set(2, 1)) shouldBe Set(2, 1)
      Monoid[Set[Int]].combine(Set(4, 3, 5), Set(5, 1)) shouldBe Set(5)
    }
  }

}
