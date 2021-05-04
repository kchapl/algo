package algo

import Sudoku.Coord.boxCoords

import scala.annotation.tailrec

object Sudoku extends App {

  sealed trait Cell { def value: Int }
  case class Given(value: Int) extends Cell
  case class Deduced(value: Int) extends Cell
  case object Empty extends Cell { val value = 0 }

  case class Coord(row: Int, col: Int)

  object Coord {

    val boxCoords: Seq[Seq[Coord]] =
      Seq(
        for {
          x <- 0 to 2
          y <- 0 to 2
        } yield Coord(x, y),
        for {
          x <- 0 to 2
          y <- 3 to 5
        } yield Coord(x, y),
        for {
          x <- 0 to 2
          y <- 6 to 8
        } yield Coord(x, y),
        for {
          x <- 3 to 5
          y <- 0 to 2
        } yield Coord(x, y),
        for {
          x <- 3 to 5
          y <- 3 to 5
        } yield Coord(x, y),
        for {
          x <- 3 to 5
          y <- 6 to 8
        } yield Coord(x, y),
        for {
          x <- 6 to 8
          y <- 0 to 2
        } yield Coord(x, y),
        for {
          x <- 6 to 8
          y <- 3 to 5
        } yield Coord(x, y),
        for {
          x <- 6 to 8
          y <- 6 to 8
        } yield Coord(x, y)
      )

    def coordsOfNextCellToFill(g: Grid): Option[Coord] = {
      val row = g.rows.indexWhere(_.contains(Empty))
      if (row == -1) None
      else Some(Coord(row, g.rows(row).indexWhere(_ == Empty)))
    }

    def coordsOfLastCellFilled(g: Grid): Option[Coord] = {
      val row = g.rows.lastIndexWhere(_.exists(_.isInstanceOf[Deduced]))
      if (row == -1) None
      else Some(Coord(row, g.rows(row).lastIndexWhere(_.isInstanceOf[Deduced])))
    }
  }

  case class Grid(rows: Seq[Array[Cell]])

  def nextCellFilled(g: Grid): Grid = {
    Coord
      .coordsOfNextCellToFill(g)
      .foreach(d => g.rows(d.row)(d.col) = Deduced(1))
    g
  }

  def incremented(g: Grid): Grid = {

    @tailrec
    def go(acc: Grid): Grid =
      if (isComplete(acc)) acc
      else {
        val lastFilled = Coord.coordsOfLastCellFilled(acc)
        lastFilled match {
          case None => acc
          case Some(last) =>
            val cellValue = acc.rows(last.row)(last.col)
            if (cellValue.value == 9) {
              acc.rows(last.row)(last.col) = Empty
              val n = Grid(acc.rows)
              go(n)
            } else {
              acc.rows(last.row)(last.col) = Deduced(
                acc.rows(last.row)(last.col).value + 1
              )
              if (isValid(acc)) acc
              else go(acc)
            }
        }
      }

    go(g)
  }

  def rowsValid(g: Grid): Boolean = g.rows.forall { row =>
    val knownValues = row.filterNot(_ == Empty)
    knownValues.map(_.value).distinct.length == knownValues.length
  }

  def colValues(g: Grid, colIndex: Int): Seq[Cell] = {
    val x = g.rows.map { row =>
      row(colIndex)
    }
    x
  }

  def colsValid(g: Grid): Boolean =
    g.rows.indices forall { colIndex =>
      val values = colValues(g, colIndex).filterNot(_ == Empty)
      values.map(_.value).distinct.length == values.length
    }

  def boxesValid(g: Grid): Boolean = boxCoords.forall { box =>
    val values = box
      .map { b =>
        g.rows(b.row)(b.col)
      }
      .filterNot(_ == Empty)
    values.map(_.value).distinct.length == values.length
  }

  def isValid(g: Grid): Boolean =
    rowsValid(g) && colsValid(g) && boxesValid(g)

  def isComplete(g: Grid): Boolean =
    (0 to 8).forall(i => !g.rows(i).contains(Empty)) && isValid(g)

  def show(g: Grid): String =
    g.rows.map { row =>
      val x = row.map {
        case Empty => "- "
        case cell  => s"${cell.value} "
      }.mkString
      s"$x\n"
    }.mkString

  def solved(g: Grid): Grid = {
    @tailrec
    def go(acc: Grid, numSteps: Int): Grid =
      if (isComplete(acc) || numSteps == 20000) {
        println(numSteps)
        acc
      } else {
        val n = nextCellFilled(acc)
        if (isValid(n)) go(n, numSteps + 1)
        else {
          val i = incremented(n)
          go(i, numSteps + 1)
        }
      }
    go(g, 0)
  }
}
