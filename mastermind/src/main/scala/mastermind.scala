/*
 * Kata description here:
 * https://gist.github.com/thunklife/14fb6b1a9d58b0e3bf8751bb4e139348
 */
import scala.util.Random

object Mastermind extends App {


  sealed trait Color
  case object Red extends Color
  case object Green extends Color
  case object Blue extends  Color
  case object Yellow extends Color

  type Code = List[Color]
  type History = List[(Code, Int, Int)]

  val allColors = List(Red, Green, Blue, Yellow)

  def checkGuess(guess: Code, actual: Code): (Int, Int) = {
    val exact = guess.zip(actual).count {case (x,y) => x == y}
    val mismatches = guess.zip(actual).filter { case (x,y) => x != y }
    val (gm, am) = mismatches.unzip
    val gmcount = gm.groupBy{x => x}.toList.map{case(k, v) => (k, v.length)}.toMap
    val amcount = am.groupBy{x => x}.toList.map{case(k, v) => (k, v.length)}.toMap

    val colorMatchCount = allColors.map{col => math.min(gmcount.getOrElse(col,0), amcount.getOrElse(col,0))}.sum


    (exact, colorMatchCount)
  }

  def randomGuess(): Code = {
    val g1 = allColors(Random.nextInt(4))
    val g2 = allColors(Random.nextInt(4))
    val g3 = allColors(Random.nextInt(4))
    val g4 = allColors(Random.nextInt(4))
    List(g1, g2, g3, g4)
  }

  def refineGuess(history: History): Code = history match {
    case Nil => randomGuess()
    case _ => randomGuess()
  }

  ////////

  //Random.setSeed(1)

  //val testCode = List(Red, Red, Green, Yellow)
  val testCode = List(Blue, Red, Green, Green)

  val testGuess = List(Red, Green, Blue, Red)
  val (e, c) = checkGuess(testGuess, testCode)
  //Console.println(e, c)

  def play(code: Code, history: History): (Boolean, History) = {
    if (history.length == 12)
      (false, history)
    else {
      val guess = refineGuess(history)
      val (e,c) = checkGuess(guess, code)
      if (e == 4)
        (true, history)
      else
      {
        play(code, (guess, e, c)::history)
      }
    }
  }

  Console.println(play(testCode, Nil))

}
