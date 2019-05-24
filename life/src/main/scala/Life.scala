/*
 * Kata description:
 * https://gist.github.com/thunklife/ee6332f858e20e2d47bd937ffa9b1f89
 */

import scala.util.Random

sealed trait State
case object Alive extends State
case object Dead extends State

case class Cell(state: State, x: Int, y: Int)

case class Grid(numRows: Int, cells: Array[Cell]) {
  val numCols: Int = cells.length / numRows
}

object Life {
  def transition(grid: Grid): Grid =
    Grid(grid.numRows, grid.cells map { cellTransition(grid, _) })

  def cellTransition(grid: Grid, cell: Cell): Cell =
    Cell(cellNextState(grid, cell), cell.x, cell.y)

  def cellNextState(grid: Grid, cell: Cell): State =
    (cell.state, liveNeighbors(grid, cell)) match {
      case (Alive, n) if n < 2  => Dead
      case (Alive, n) if n <= 3 => Alive
      case (Alive, _)           => Dead
      case (Dead, n) if n == 3  => Alive
      case (Dead, _)            => Dead
    }

  def liveNeighbors(grid: Grid, cell: Cell): Int = neighbors(grid, cell) count isAlive

  def isAlive(cell: Cell): Boolean = cell.state == Alive

  // Cell data is stored in row-major order
  def cell(grid: Grid, x: Int, y: Int): Option[Cell] = {
    if (x < 0 || x >= grid.numCols || y < 0 || y >= grid.numRows) None
    else Some(grid.cells(y * grid.numCols + x))
  }

  def neighbors(grid: Grid, c: Cell): List[Cell] = {
    val x = c.x
    val y = c.y

    val north = cell(grid, x, y - 1)
    val south = cell(grid, x, y + 1)
    val east = cell(grid, x + 1, y)
    val west = cell(grid, x - 1, y)

    val nw = cell(grid, x - 1, y - 1)
    val ne = cell(grid, x + 1, y - 1)
    val sw = cell(grid, x - 1, y + 1)
    val se = cell(grid, x + 1, y + 1)

    List(north, south, east, west, nw, ne, sw, se).flatten
  }
}

/// TESTING

object LifeApp extends App {
  def initialGrid(): Grid = {
    val SIZE: Int = 5

    val rand = new Random()
    val cells = Array.tabulate(SIZE * SIZE) { i =>
      val alive = rand.nextBoolean()
      val state = if (alive) Alive else Dead
      Cell(state, i % SIZE, i / SIZE)
    }

    /*
    val grid = Array.ofDim[Cell](SIZE * SIZE)
    grid(0) = Cell(Alive, 0, 0)
    grid(1) = Cell(Alive, 1, 0)
    grid(2) = Cell(Alive, 2, 0)
    grid(3) = Cell(Alive, 3, 0)
    grid(4) = Cell(Dead, 4, 0)

    grid(5) = Cell(Alive, 0, 1)
    grid(6) = Cell(Alive, 1, 1)
    grid(7) = Cell(Dead, 2, 1)
    grid(8) = Cell(Alive, 3, 1)
    grid(9) = Cell(Alive, 4, 1)

    grid(10) = Cell(Alive, 0, 2)
    grid(11) = Cell(Dead, 1, 2)
    grid(12) = Cell(Dead, 2, 2)
    grid(13) = Cell(Dead, 3, 2)
    grid(14) = Cell(Alive, 4, 2)

    grid(15) = Cell(Dead, 0, 3)
    grid(16) = Cell(Alive, 1, 3)
    grid(17) = Cell(Alive, 2, 3)
    grid(18) = Cell(Dead, 3, 3)
    grid(19) = Cell(Dead, 4, 3)

    grid(20) = Cell(Dead, 0, 4)
    grid(21) = Cell(Dead, 1, 4)
    grid(22) = Cell(Alive, 2, 4)
    grid(23) = Cell(Dead, 3, 4)
    grid(24) = Cell(Dead, 4, 4)

     */
    Grid(SIZE, cells)
  }

  override def main(args: Array[String]): Unit = {
    val g = initialGrid()
    g.cells map println

    /*
    println("===")
    val t = Array.tabulate[Option[Cell]](g.length) { i =>
      val x = i % 5
      val y = i / 5
      Life.cell(g, x, y)
    }
    t map println

    println("***")

    println(Life.cell(g, 0, 2))
    val n = Life.neighbors(g, 0, 1)
    println(n)
*/

    println("---")

    val n = Life.transition(g)
    n.cells map println

  }
}
