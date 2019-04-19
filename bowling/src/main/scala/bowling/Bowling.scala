/*
 *  Kata instructions here:
 *  https://gist.github.com/thunklife/08b14acba73bff430e8a16f256eef166
 */

package bowling

object Bowling {
  case class Roll(score: Int)

  sealed trait Frame
  object Strike extends Frame
  case class Spare(firstRoll: Roll) extends Frame
  case class Missed(first: Roll, second: Roll) extends Frame

  def score(frames: List[Frame], bonus: List[Roll]): Int = {
    val allRolls = frames.flatMap {
      _ match {
        case Missed(s1, s2) => List(s1, s2)
        case Spare(s) => List(s, Roll(10 - s.score))
        case Strike => List(Roll(10))
      }
    } ++ bonus

    def score_frame(f: Frame, rollsFromHere: List[Roll]): (Int, List[Roll]) = f match {
      case Missed(s1, s2) =>
        val remaining = rollsFromHere.drop(2)
        (s1.score + s2.score, remaining)
      case Spare(_) =>
        val remaining = rollsFromHere.drop(2)
        (10 + remaining.head.score, remaining)
      case Strike =>
        val remaining = rollsFromHere.drop(1)
        (10 + remaining.head.score + remaining.tail.head.score, remaining)
    }

    def score_helper(framesHelper: List[Frame], rolls: List[Roll], acc: Int): Int =
      framesHelper match {
        case Nil => acc
        case h::t =>
          val (frame_score, remaining) = score_frame(h, rolls)
          score_helper(t, remaining, acc + frame_score)
      }

    score_helper(frames, allRolls, 0)
  }

  def main(args: Array[String]): Unit = {
    val test1 = (List.fill(10)(Strike), List.fill(2)(Roll(10)))
    val test2 = (List.fill(10)(Missed(Roll(9),Roll(0))), Nil)
    val test3 = (List.fill(10)(Spare(Roll(5))), List(Roll(5)))

    println(score(test1._1, test1._2))
    println(score(test2._1, test2._2))
    println(score(test3._1, test3._2))
  }
}