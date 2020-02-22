/*
 * Kata description here:
 * https://gist.github.com/thunklife/61ec848bb5168e1cf123628b07203fa8
 */
import java.io.EOFException

import scala.collection.{AbstractIterable, AbstractIterator}
import scala.io.{Source, StdIn}
import scala.util.Random

class Guesses extends Iterable[Char] {
  @scala.annotation.tailrec
  private def nextGuess(): Char = {
    try {
      print("Enter your next guess: ")
      StdIn.readChar()
    } catch {
      case _: EOFException => nextGuess()
      case _: StringIndexOutOfBoundsException => nextGuess()
    }
  }

  override def iterator: Iterator[Char] = new AbstractIterator[Char] {
    override def hasNext: Boolean = true
    override def next(): Char = nextGuess()
  }
}

class StateGen[I, N, S](initial: I => S)(transition: (S, N) => S)(seed: I) {    // TODO can I replace this with anything built in?  Iterator.unfold?
  def run(inputs: Iterable[N]): Iterable[S] = new AbstractIterable[S] {
    override def iterator: Iterator[S] = new AbstractIterator[S] {
      private[this] var state: S = null.asInstanceOf[S]  // FIXME!
      private[this] val underlying = inputs.iterator

      override def hasNext: Boolean = {
        if (state == null) true
        else underlying.hasNext
      }
      override def next(): S = {
        if (state == null) {
          state = initial(seed)
        } else {
          val n = underlying.next()
          state = transition(state, n)
        }
        state
      }
    }
  }
}


object Hangman {
  sealed trait TerminationState
  case object InProgress extends TerminationState
  case object Won extends TerminationState
  case object Lost extends TerminationState

  type WordState = List[Option[Char]]

  case class GameState(
                        word: String,
                        badGuesses: Int,
                        termState: TerminationState,
                        wordState: WordState,
                        guessedSoFar: Set[Char])

  private[this] val gallows: Array[String] = Array(
    "  ---\n |  |\n    |\n    |\n    |\n    |\n ___|___",
    "  ---\n |  |\n o  |\n    |\n    |\n    |\n ___|___",
    "  ---\n |  |\n o  |\n |  |\n    |\n    |\n ___|___",
    "  ---\n |  |\n o  |\n\\|  |\n    |\n    |\n ___|___",
    "  ---\n |  |\n o  |\n\\|/ |\n    |\n    |\n ___|___",
    "  ---\n |  |\n o  |\n\\|/ |\n/   |\n    |\n ___|___",
    "  ---\n |  |\n o  |\n\\|/ |\n/ \\ |\n    |\n ___|___",
  )

  private[this] val MAX_GUESSES = gallows.length - 1

  def selectRandomWord(): String = {
    val infile = Source.fromFile("word-list.txt")
    val words = infile.getLines().toArray
    val rnd = new Random()
    val index = rnd.nextInt(words.length)
    words(index)
  }

  def initialGameState(word: String): GameState =
    GameState(word, 0, InProgress, List.fill(word.length)(None), Set.empty)

  def nextGameState(state: GameState, guess: Char): GameState = {
    if (state.word.contains(guess)) {
      // zip word with state.wordState, update where letter matches guess
      val zipped = state.word.toList zip state.wordState
      val wordState = zipped map {
        case (c, _) if c == guess => Some(c)
        case (_, ws) => ws
      }
      def hasWon(s: WordState) = !s.contains(None)
      val termState = if (hasWon(wordState)) Won else state.termState
      state.copy(wordState=wordState, termState=termState, guessedSoFar=state.guessedSoFar + guess)
    } else {
      val badGuesses = state.badGuesses + 1
      val termState = if (badGuesses == MAX_GUESSES) Lost else state.termState
      state.copy(badGuesses=badGuesses, termState=termState, guessedSoFar=state.guessedSoFar + guess)
    }
  }

  def wordDisplay(word: List[Option[Char]]): String = word.map {
    case Some(c) => s"$c "
    case None => "_ "
  }.reduce(_ + _)

  def gallowsDisplay(badGuesses: Int): String = gallows(badGuesses)

  def showGameState(state: GameState): Unit = {
    println(gallowsDisplay(state.badGuesses))
    println()
    println(wordDisplay(state.wordState))
    println(s"Guessed so far: ${new String(state.guessedSoFar.toArray.sorted)}")

    state.termState match {
      case Won =>
        println("You win!")
      case Lost =>
        println("You lose!")
        println(s"The word was ${state.word}")
      case _ => ()
    }
  }

  def main(args: Array[String]): Unit = {
    val word = selectRandomWord()    // TODO why is type inference failing for 'Char' ?
    val sg: StateGen[String, Char, GameState] = new StateGen(initialGameState)(nextGameState)(word)

    //println(word)   // debugging
    val stream = sg.run(new Guesses).iterator

    stream.foreach { state =>
      showGameState(state)
      if (state.termState != InProgress)
        return
    }


    // TODO this does not take the last element (Won/Lost)
    //stream.takeWhile(_.termState == InProgress).foreach(showGameState)

  }

}
