package algo

import sudoku.Grid

import scala.annotation.tailrec

object Sudoku extends App {

  sealed trait Cell { def value: Int }
  case class Given(value: Int) extends Cell
  case class Deduced(value: Int) extends Cell
  case object Empty extends Cell { val value = 0 }

  case class Coord(row: Int, col: Int)

  object Coord {

    val boxCoords: Seq[Seq[Coord]] = {
      val coords = (0 to 8).sliding(3, 3).toSeq
      for {
        rowCoords <- coords
        colCoords <- coords
      } yield for {
        x <- rowCoords
        y <- colCoords
      } yield Coord(x, y)
    }
  }

  def updated(g: Grid, c: Coord, value: Cell): Grid =
    Grid(g.rows.updated(c.row, g.rows(c.row).updated(c.col, value)))

  def incremented(g: Grid): Grid = {

    @tailrec
    def go(acc: Grid): Grid =
      if (acc.isComplete) acc
      else {
        val lastFilled = acc.coordsOfLastCellFilled()
        lastFilled match {
          case None => acc
          case Some(last) =>
            val cellValue = acc.rows(last.row)(last.col)
            if (cellValue.value == 9)
              go(updated(acc, last, Empty))
            else {
              val x = updated(
                acc,
                last,
                Deduced(acc.rows(last.row)(last.col).value + 1)
              )
              if (x.isValid) x
              else go(x)
            }
        }
      }

    go(g)
  }

  def allUnique(cells: Seq[Cell]): Boolean =
    cells.map(_.value).distinct.length == cells.length

  def colValues(g: Grid, colIndex: Int): Seq[Cell] =
    g.rows.map(_(colIndex))

  def solved(g: Grid): Grid = {
    @tailrec
    def go(acc: Grid, numSteps: Int): Grid =
      if (acc.isComplete || numSteps == 20000) {
        println(numSteps)
        acc
      } else {
        val n = acc.nextCellFilled
        if (n.isValid) go(n, numSteps + 1)
        else
          go(incremented(n), numSteps + 1)
      }
    go(g, 0)
  }
}
