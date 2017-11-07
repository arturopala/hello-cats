import cats.data.Validated
import cats.implicits._

case class User(name: String,age: Int)

object User {

  def readUser(input: Map[String, String]): Validated[List[String], User] = {
    readName(input).product(readAge(input)).map { case (n, a) => User(n, a) }
  }

  def readName(input: Map[String, String]): Validated[List[String], String] = {
    getValue(input, "name").andThen(nonBlank)
  }

  def readAge(input: Map[String, String]): Validated[List[String], Int] = {
    getValue(input, "age").andThen(parseInt).andThen(nonNegative)
  }

  def getValue(input: Map[String, String], key: String): Validated[List[String], String] = {
    Validated.fromOption(input.get(key), s"missing input key $key".pure[List])
  }

  def nonBlank(text: String): Validated[List[String], String] = {
    text.valid[List[String]].ensure("empty string".pure[List])(_.trim.nonEmpty)
  }

  def parseInt(age: String): Validated[List[String], Int] = {
    Validated.catchOnly[NumberFormatException](age.toInt).leftMap(_ => s"non-integer value".pure[List])
  }

  def nonNegative(num: Int): Validated[List[String], Int] = {
    num.valid[List[String]].ensure("negative integer".pure[List])(_ >= 0)
  }
}

