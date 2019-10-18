import matryoshka.Algebra
import matryoshka.implicits.toRecursiveOps
import matryoshka.data.Fix
import scalaz.Functor

sealed trait Razor[A]

object Razor {
  final case class Lit[A](value: Int) extends Razor[A]
  final case class Add[A](l: A, r: A) extends Razor[A]

  implicit val razorFunctor: Functor[Razor] = new Functor[Razor] {
    override def map[A, B](fa: Razor[A])(f: (A) => B): Razor[B] = fa match {
      case Lit(value) => Lit[B](value)
      case Add(l, r) => Add(f(l), f(r))
    }
  }

  val evaluator: Algebra[Razor, Int] = {
    case Lit(x)    => x
    case Add(x, y) => x + y
  }

  val prettifier: Algebra[Razor, String] = {
    case Lit(x)    => x.toString
    case Add(x, y) => s"($x + $y)"
  }

  type Expr = Fix[Razor]

  def lit(value: Int): Expr = Fix(Lit(value))
  def add(x: Expr, y: Expr): Expr = Fix(Add(x, y))

  def interpret(expr: Expr): Int = expr.cata(evaluator)
  def pretty(expr: Expr): String = expr.cata(prettifier)
}

object HuttonsRazorSample extends App {
  import Razor._

  // Sample test
  def onePlusTwo: Expr = add(lit(1), lit(2))

  println(interpret(onePlusTwo))
  println(pretty(onePlusTwo))
}
