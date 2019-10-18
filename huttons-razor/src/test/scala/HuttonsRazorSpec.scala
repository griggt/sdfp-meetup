import org.scalatest._
import Razor._

class HuttonsRazorSpec extends FlatSpec with Matchers {
  def onePlusTwo: Expr = add(lit(1), lit(2))

  "add(lit(1), lit(2))" should "evaluate to 3" in {
    interpret(onePlusTwo) shouldBe 3
  }

  it should "pretty print as (1 + 2)" in {
    pretty(onePlusTwo) shouldBe "(1 + 2)"
  }

}
