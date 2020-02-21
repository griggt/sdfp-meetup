/*
 * Kata description here:
 * https://gist.github.com/thunklife/61ec848bb5168e1cf123628b07203fa8
 */
import java.io.EOFException

import scala.io.{Source, StdIn}
import scala.util.Random

/*
 *  Gallows
 *     ___        ___        ___       ___        ___        ___          ___
 *    |  |       |  |       |  |      |  |       |  |       |  |        |  |
 *       |       o  |       o  |      o  |       o  |       o  |        o  |
 *       |          |       |  |     \|  |      \|/ |      \|/ |       \|/ |
 *       |          |          |         |          |      /   |       / \ |
 *    ___|___    ___|___    ___|___   ___|___    ___|___    ___|___     ___|___
 *
 */


object Hangman {
  sealed trait TerminationState
  case object InProgress extends TerminationState
  case object Won extends TerminationState
  case object Lost extends TerminationState

  case class GameState(badGuesses: Int, wordState: List[Option[Char]], guessedSoFar: Set[Char])

  val MAX_GUESSES = 6

  val gallows: Array[String] = Array(
    "  ---\n |  |\n    |\n    |\n    |\n    |\n ___|___",
    "  ---\n |  |\n o  |\n    |\n    |\n    |\n ___|___",
    "  ---\n |  |\n o  |\n |  |\n    |\n    |\n ___|___",
    "  ---\n |  |\n o  |\n\\|  |\n    |\n    |\n ___|___",
    "  ---\n |  |\n o  |\n\\|/ |\n    |\n    |\n ___|___",
    "  ---\n |  |\n o  |\n\\|/ |\n/   |\n    |\n ___|___",
    "  ---\n |  |\n o  |\n\\|/ |\n/ \\ |\n    |\n ___|___",
  )

  def selectRandomWord(): String = {
    val infile = Source.fromFile("word-list.txt")
    val words = infile.getLines().toArray
    val rnd = new Random()
    val index = rnd.nextInt(words.length)
    words(index)
  }

  def initialGameState(word: String): GameState = GameState(0, List.fill(word.length)(None), Set.empty)

  def nextGameState(word: String, state: GameState, guess: Char): GameState = {
    if (word.contains(guess)) {
      // zip word with state.wordState, update where letter matches guess
      val zipped = word.toList zip state.wordState
      val newWordState = zipped map {
        case (c, _) if c == guess => Some(c)
        case (_, ws) => ws
      }
      GameState(state.badGuesses, newWordState, state.guessedSoFar + guess)
    } else {
      GameState(state.badGuesses + 1, state.wordState, state.guessedSoFar + guess)
    }
  }

  def hasWon(state: GameState): Boolean = !state.wordState.contains(None)

  def terminationState(state: GameState): TerminationState = {
    if (hasWon(state)) Won
    else if (state.badGuesses == MAX_GUESSES) Lost
    else InProgress
  }

  // State:s
  //  number of incorrect guesses
  //  correct guesses by position

  def wordStateDisplay(word: List[Option[Char]]): String = word.map {
      case Some(c) => s"$c "
      case None => "_ "
    }.reduce(_ + _)

  def gallowsState(badGuesses: Int): String = gallows(badGuesses)

  def showGameState(state: GameState): Unit = {
    println(gallowsState(state.badGuesses))
    println()
    println(wordStateDisplay(state.wordState))
    println(s"Guessed so far: ${new String(state.guessedSoFar.toArray.sorted)}")
  }

  @scala.annotation.tailrec
  def nextGuess(): Char = {
    try {
      StdIn.readChar()
    } catch {
      case _: EOFException => nextGuess()
      case _: StringIndexOutOfBoundsException => nextGuess()
    }
  }

  def main(args: Array[String]): Unit = {
    val word = selectRandomWord()
    var state = initialGameState(word)

    //println(word)   // debugging
    showGameState(state)

    while (true) {
      print("Enter your next guess: ")
      val guess = nextGuess()

      state = nextGameState(word, state, guess)
      showGameState(state)

      terminationState(state) match {
        case Won =>
          println("You win!")
          return
        case Lost =>
          println("You lose!")
          println(s"The word was $word")
          return
        case _ => ()
      }
    }
  }

  type Initial[I, S] = I => S
  type Transition[N, S] = (S, N) => S

  // create an Iterable[N] to generate the game input (per-letter user input)

}
