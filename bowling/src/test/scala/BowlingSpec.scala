import bowling._
import org.scalatest._

class BowlingSpec extends FlatSpec with Matchers {
  "A game" should "score 300 if all strikes are bowled" in {
    val frames = List.fill(10)(Strike)
    val bonusRolls = List.fill(2)(Roll(10))
    Bowling.score(frames, bonusRolls) should be (300)
  }

  it should "score 90 if all 9- are bowled" in {
    val frames = List.fill(10)(Missed(Roll(9), Roll(0)))
    val bonusRolls = Nil
    Bowling.score(frames, bonusRolls) should be (90)
  }

  it should "score 150 if all 5/ are bowled" in {
    val frames = List.fill(10)(Spare(Roll(5)))
    val bonusRolls = List(Roll(5))
    Bowling.score(frames, bonusRolls) should be (150)
  }

}
