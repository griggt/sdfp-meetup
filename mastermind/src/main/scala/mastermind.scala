/*
 * Kata description here:
 * https://gist.github.com/thunklife/14fb6b1a9d58b0e3bf8751bb4e139348
 */
import scala.util.Random

object Mastermind extends App {
  sealed trait Color
  case object Red extends Color
  case object Green extends Color
  case object Blue extends Color
  case object Yellow extends Color

  type Code = List[Color]
  type History = List[(Code, Int, Int)]

  final val MAX_GUESSES = 12

  val allColors = List(Red, Green, Blue, Yellow)

  def evaluate(guess: Code, actual: Code): (Int, Int) = {
    def counts[T](s: List[T]) = s.groupBy(x => x).view.mapValues(_.length)
    def colorCount(code: Code, color: Color) = counts(code).getOrElse(color, 0)
    def matchingColorCount(c1: Code, c2: Code)(color: Color) = math.min(colorCount(c1, color), colorCount(c2, color))

    val zipped = guess.zip(actual)
    val mismatches = zipped.filter { case (x, y) => x != y }
    val countColorMatches = (matchingColorCount _).tupled(mismatches.unzip)

    val exactMatchCount = zipped.count { case (x, y) => x == y }
    val colorMatchCount = allColors.map(countColorMatches).sum

    (exactMatchCount, colorMatchCount)
  }

  def randomColor: Color = allColors(Random.nextInt(4))

  def randomGuess(): Code = List.fill(4)(randomColor)

  def refineGuess(history: History): Code = history match {
    case Nil => randomGuess()
    case _ => randomGuess()
  }

  def play(code: Code, history: History): (Boolean, History) = history.length match {
    case MAX_GUESSES => (false, history)
    case _ =>
      val guess = refineGuess(history)
      val (exactMatches, colorMatches) = evaluate(guess, code)

      if (exactMatches == code.length)
        (true, history)
      else
        play(code, (guess, exactMatches, colorMatches) :: history)
  }

  //////// TESTING

  //Random.setSeed(1)

  val testCode = List(Red, Red, Green, Yellow)
  //val testCode = List(Blue, Red, Green, Green)
  val testGuess = List(Red, Green, Blue, Red)
  val (e, c) = evaluate(testGuess, testCode)
  println(e, c)

  // Test game play

  println(play(testCode, Nil))
}
