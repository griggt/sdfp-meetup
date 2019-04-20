/*
 *  Kata instructions here:
 *  https://gist.github.com/thunklife/08b14acba73bff430e8a16f256eef166
 */

package bowling

case class Roll(pins: Int)

sealed trait Frame {
  val numRolls: Int
  val numBonusRolls: Int
  def baseScore: Int
}

case object Strike extends Frame {
  override val numRolls = 1
  override val numBonusRolls = 2
  override val baseScore = 10
}

case class Spare(firstRoll: Roll) extends Frame {
  override val numRolls = 2
  override val numBonusRolls = 1
  override val baseScore = 10
}

case class Missed(first: Roll, second: Roll) extends Frame {
  override val numRolls = 2
  override val numBonusRolls = 0
  override def baseScore: Int = first.pins + second.pins
}

object Bowling {

  def score(frames: List[Frame], bonus: List[Roll]): Int = {

    def scoreFrame(frame: Frame, rolls: List[Roll]): (Int, List[Roll]) = {
      val futureRolls = rolls drop frame.numRolls
      val bonusScore = futureRolls.take(frame.numBonusRolls).map(_.pins).sum
      (frame.baseScore + bonusScore, futureRolls)
    }

    def scoreRecursive(remainingFrames: List[Frame], rolls: List[Roll], acc: Int): Int =
      remainingFrames match {
        case Nil => acc
        case f::fs =>
          val (frameScore, futureRolls) = scoreFrame(f, rolls)
          scoreRecursive(fs, futureRolls, acc + frameScore)
      }

    val allRolls = (frames flatMap {
      case Strike => List(Roll(10))
      case Spare(r) => List(r, Roll(10 - r.pins))
      case Missed(r1, r2) => List(r1, r2)
    }) ++ bonus

    scoreRecursive(frames, allRolls, 0)
  }

  // Parsing string input

  def rollValue(s: String): Roll = s match {
    case "-" => Roll(0)
    case "X" => Roll(10)
    case _ => Roll(s.toInt)
  }

  private val StrikeRE = "X".r
  private val SpareRE  = "([1-9-])/".r
  private val MissedRE = "([1-9-])([1-9-])".r

  private val SpareBonusRE = "([1-9-])/([X1-9-])".r
  private val StrikeBonusRE = "X([X1-9-])([X1-9-])".r
  private val StrikeBonusSpareRE = "X([1-9-])/".r

  def parseSingleFrame(s: String): (Frame, List[Roll]) = s match {
    case StrikeRE() => (Strike, Nil)
    case SpareRE(r1) => (Spare(rollValue(r1)), Nil)
    case MissedRE(r1, r2) => (Missed(rollValue(r1), rollValue(r2)), Nil)
    case SpareBonusRE(r1, b1) => (Spare(rollValue(r1)), List(rollValue(b1)))
    case StrikeBonusRE(b1, b2) => (Strike, List(rollValue(b1), rollValue(b2)))
    case StrikeBonusSpareRE(b1) =>
      val r1 = rollValue(b1)
      (Strike, List(r1, Roll(10 - r1.pins)))
  }

  def parseAllFrames(s: String): (List[Frame], List[Roll]) = {
    val (frames, bonusNested) = s.split(' ').toList.map(parseSingleFrame).unzip
    (frames, bonusNested.flatten)
  }

  def score(s: String): Int = {
    val (frames, bonus) = parseAllFrames(s)
    score(frames, bonus)
  }

}
