package algo
package sudoku

import Sudoku.Coord.boxCoords
import Sudoku._

case class Grid(rows: Seq[Seq[Cell]]) {

  def coordsOfLastCellFilled(): Option[Coord] = {
    val row = rows.lastIndexWhere(_.exists(_.isInstanceOf[Deduced]))
    if (row == -1) None
    else Some(Coord(row, rows(row).lastIndexWhere(_.isInstanceOf[Deduced])))
  }

  lazy val nextCellFilled: Grid = {
    val coordsOfNextCellToFill: Option[Coord] = {
      val row = rows.indexWhere(_.contains(Empty))
      if (row == -1) None
      else Some(Coord(row, rows(row).indexWhere(_ == Empty)))
    }
    coordsOfNextCellToFill
      .map(d => updated(this, d, Deduced(1)))
      .getOrElse(this)
  }

  lazy val isValid: Boolean = {
    val rowsValid: Boolean = rows.forall { row =>
      allUnique(row.filterNot(_ == Empty))
    }
    val colsValid: Boolean =
      rows.indices forall { colIndex =>
        val values = colValues(this, colIndex).filterNot(_ == Empty)
        allUnique(values)
      }
    val boxesValid: Boolean = boxCoords.forall { box =>
      val values = box
        .map(b => rows(b.row)(b.col))
        .filterNot(_ == Empty)
      allUnique(values)
    }
    rowsValid && colsValid && boxesValid
  }

  lazy val isComplete: Boolean =
    (0 to 8).forall(i => !rows(i).contains(Empty)) && isValid

  lazy val show: String =
    rows.map { row =>
      val x = row.map {
        case Empty => "- "
        case cell  => s"${cell.value} "
      }.mkString
      s"$x\n"
    }.mkString
}
