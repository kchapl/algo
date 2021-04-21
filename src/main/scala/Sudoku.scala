package algo

object Sudoku extends App {

  case class Coord(row: Int, col: Int)
  case class Grid(
      rows: Array[Array[Int]],
      filled: Seq[Coord]
  )

  object Grid {
    def apply(initCells: Array[Array[Int]]): Grid =
      Grid(initCells, Nil)
  }

  def nextCellFilled(g: Grid): Grid = {
    val coordsOfNextCellToFill = {
      def f(rowIndex: Int): Int = g.rows(rowIndex).indexWhere(_ == 0)
      val row = g.rows.indexWhere(row => row.contains(0))
      val h = f(row)
      Coord(row, h)
    }
    g.rows(coordsOfNextCellToFill.row)(coordsOfNextCellToFill.col) = 1
    val h = g.copy(filled = g.filled :+ coordsOfNextCellToFill)
    h
  }

  def incremented(g: Grid): Grid = {
    val h = g.filled.last
    val cellValue = g.rows(h.row)(h.col)
    if (cellValue == 9) {
      val j = g.filled(g.filled.length - 2)
      val c = g.rows(j.row)(j.col)
      g.rows(j.row)(j.col) = c + 1
    } else g.rows(h.row)(h.col) = cellValue + 1
    g
  }

  def rowsValid(g: Grid): Boolean = g.rows.forall { row =>
    val knownValues = row.filter(_ > 0)
    knownValues.distinct.length == knownValues.length
  }

  def colValues(g: Grid, colIndex: Int): Seq[Int] = {
    val x = g.rows.map { row =>
      row(colIndex)
    }
    x
  }

  def colsValid(g: Grid): Boolean =
    g.rows.indices forall { colIndex =>
      val values = colValues(g, colIndex).filter(_ > 0)
      values.distinct.length == values.length
    }

  def boxCoords(g: Grid): Seq[Seq[Coord]] =
    Seq(
      Seq(Coord(0, 0), Coord(0, 1), Coord(1, 0), Coord(1, 1)),
      Seq(Coord(0, 2), Coord(0, 3), Coord(1, 2), Coord(1, 3)),
      Seq(Coord(2, 0), Coord(2, 1), Coord(3, 0), Coord(3, 1)),
      Seq(Coord(2, 2), Coord(2, 3), Coord(3, 2), Coord(3, 3))
    )

  def boxesValid(g: Grid): Boolean = boxCoords(g).forall { box =>
    val values = box
      .map { b =>
        g.rows(b.row)(b.col)
      }
      .filter(_ > 0)
    values.distinct.length == values.length
  }

  def isValid(g: Grid): Boolean =
    rowsValid(g) && colsValid(g) && boxesValid(g)

  def isComplete(g: Grid): Boolean =
    !g.rows(0).contains(0) &&
      !g.rows(1).contains(0) &&
      !g.rows(2).contains(0) &&
      !g.rows(3).contains(0)

  val initGrid = Grid(
    Array(
      Array(1, 2, 3, 4),
      Array(4, 3, 2, 1),
      Array(3, 4, 1, 2),
      Array(2, 1, 0, 0)
    )
  )

  var grid = initGrid

  var count = 0

  def printGrid(g: Grid): Unit = {
    g.rows.foreach { row =>
      row.foreach { cell =>
        print(cell)
        print(" ")
      }
      println()
    }
  }

  println("start")
  printGrid(grid)
  while (!isComplete(grid) && count < 10) {
    grid = nextCellFilled(grid)
    var count2 = 0
    while (!isValid(grid) && count2 < 10) {
      grid = incremented(grid)
      count2 = count2 + 1
    }
    count = count + 1
  }
  println()
  println("end")
  printGrid(grid)
}
