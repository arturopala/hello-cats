import cats.data.Validated.{Invalid, Valid}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FeatureSpec, Matchers}

class ValidatedUserSpec extends FeatureSpec with Matchers with PropertyChecks {

  val nonEmptyString = Gen.alphaStr.suchThat(_.nonEmpty)
  val twoDigitString = Gen.listOfN(2, Gen.oneOf(1,2,3,4,5,6,7,8,9)).map(_.mkString)

  feature("Creating User entity from string input") {

    scenario("valid if all input values are valid") {
      forAll(nonEmptyString, twoDigitString) { (a, b) =>
        User.readUser(Map("name" -> a, "age" -> b)) shouldBe Valid(User(a,b.toInt))
      }
    }

    scenario("invalid if name is empty") {
      forAll(twoDigitString) { (b) =>
        User.readUser(Map("name" -> "", "age" -> b)) shouldBe Invalid(List("empty string"))
      }
    }

    scenario("invalid if age is not number") {
      forAll(nonEmptyString, nonEmptyString) { (a, b) =>
        User.readUser(Map("name" -> a, "age" -> b)) shouldBe Invalid(List("non-integer value"))
      }
    }

    scenario("invalid if age is negative") {
      forAll(nonEmptyString, twoDigitString) { (a, b) =>
        User.readUser(Map("name" -> a, "age" -> s"-$b")) shouldBe Invalid(List("negative integer"))
      }
    }

    scenario("invalid if name is empty and age is negative") {
      forAll(twoDigitString) { (b) =>
        User.readUser(Map("name" -> "", "age" -> s"-$b")) shouldBe Invalid(List("empty string", "negative integer"))
      }
    }

  }

}
