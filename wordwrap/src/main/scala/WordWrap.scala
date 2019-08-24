/*
 * From https://gist.github.com/thunklife/4cbb85d6a011a5a12638deb2875730b3
 *
 * Write a function, wrap that takes two arguments, a string, and a column number.
 * The function returns the string, but with line breaks inserted in the right places
 * based column number passed to the function.
 *
 * Make sure that no line is longer than the column number and only break lines at word boundaries.
 *
 * Like a word processor, break the line by replacing the last space in a line with a newline.
 */

object WordWrap extends App {

  /*
  def break(pos: Int): String = str.substring (0, pos) + '\n' + wrap(str.substring (pos + 1), col)

  if (str.length <= col)
    str
  else {
    val breakPos = str.lastIndexWhere(char => char.isWhitespace, col)
    if (breakPos < 0) { // no suitable whitespace character to break on
      // find the next whitespace character and use that
      val firstAvailableBreak = str.indexWhere(char => char.isWhitespace, col + 1)
      if (firstAvailableBreak < 0) str else break(firstAvailableBreak)
    }
    else
      break(breakPos)
  }
  */


  def wrap(str: String, col: Int): String = {
    // General idea:
    //   - if string length is <= col, no breaking is necessary; otherwise:
    //   - find last word break at position <= column number
    //   - return [chars before the break] + '\n' + recursive call of chars after the break

    def break(pos: Int): String = pos match {
      case -1 => str
      case _  => str.substring(0, pos) + '\n' + wrap(str.substring(pos + 1), col)
    }

    val breakPos = {
      if (str.length <= col)
        -1
      else {
        val p = str.lastIndexWhere(char => char.isWhitespace, col)
        if (p >= 0) p else str.indexWhere(char => char.isWhitespace, col + 1)
      }
    }

    break(breakPos)
  }

  // TESTING

  val testString = "Write a function, wrap that takes two arguments, a string, and a column number. The function returns the string, but with line breaks inserted in the right places based column number passed to the function. Make sure that no line is longer than the column number and only break lines at word boundaries."

  println(testString)
  println(wrap(testString, 79))

  println(wrap("abcdefghjik lmnopqrstuvwxyz", 10)) // hmm, should this break or not?

}
