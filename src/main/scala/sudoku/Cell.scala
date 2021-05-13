package algo
package sudoku

sealed trait Cell { def value: Int }

case class Given(value: Int) extends Cell

case class Deduced(value: Int) extends Cell

case object Empty extends Cell { val value = 0 }
