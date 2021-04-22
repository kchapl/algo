package algo

import Sudoku.{Deduced, Empty, Given, Grid, show}

import utest._

object SodukoTestSuite extends TestSuite {
  val tests: Tests = Tests {
    test("solved") {
      val problem = Grid(
        Array(
          Array(
            Given(1),
            Empty,
            Given(5),
            Given(3),
            Empty,
            Empty,
            Given(7),
            Empty,
            Given(4)
          ),
          Array(
            Empty,
            Empty,
            Given(4),
            Given(8),
            Given(5),
            Empty,
            Given(6),
            Empty,
            Empty
          ),
          Array(
            Given(9),
            Given(2),
            Empty,
            Empty,
            Empty,
            Empty,
            Given(8),
            Empty,
            Empty
          ),
          Array(
            Empty,
            Empty,
            Empty,
            Empty,
            Empty,
            Given(4),
            Empty,
            Empty,
            Given(1)
          ),
          Array(
            Empty,
            Empty,
            Given(8),
            Empty,
            Empty,
            Empty,
            Given(2),
            Empty,
            Empty
          ),
          Array(
            Empty,
            Given(3),
            Empty,
            Empty,
            Empty,
            Given(1),
            Empty,
            Given(5),
            Given(9)
          ),
          Array(
            Given(2),
            Given(9),
            Given(1),
            Empty,
            Given(6),
            Empty,
            Empty,
            Empty,
            Empty
          ),
          Array(
            Empty,
            Empty,
            Empty,
            Given(5),
            Given(9),
            Empty,
            Empty,
            Given(2),
            Empty
          ),
          Array(
            Given(6),
            Empty,
            Empty,
            Given(7),
            Empty,
            Empty,
            Empty,
            Empty,
            Empty
          )
        )
      )
      val solution = Grid(
        Array(
          Array(
            Given(1),
            Deduced(8),
            Given(5),
            Given(3),
            Deduced(2),
            Deduced(6),
            Given(7),
            Deduced(9),
            Given(4)
          ),
          Array(
            Deduced(3),
            Deduced(7),
            Given(4),
            Given(8),
            Given(5),
            Deduced(9),
            Given(6),
            Deduced(1),
            Deduced(2)
          ),
          Array(
            Given(9),
            Given(2),
            Deduced(6),
            Deduced(1),
            Deduced(4),
            Deduced(7),
            Given(8),
            Deduced(3),
            Deduced(5)
          ),
          Array(
            Deduced(5),
            Deduced(6),
            Deduced(9),
            Deduced(2),
            Deduced(7),
            Given(4),
            Deduced(3),
            Deduced(8),
            Given(1)
          ),
          Array(
            Deduced(4),
            Deduced(1),
            Given(8),
            Deduced(9),
            Deduced(3),
            Deduced(5),
            Given(2),
            Deduced(6),
            Deduced(7)
          ),
          Array(
            Deduced(7),
            Given(3),
            Deduced(2),
            Deduced(6),
            Deduced(8),
            Given(1),
            Deduced(4),
            Given(5),
            Given(9)
          ),
          Array(
            Given(2),
            Given(9),
            Given(1),
            Deduced(4),
            Given(6),
            Deduced(8),
            Deduced(5),
            Deduced(7),
            Deduced(3)
          ),
          Array(
            Deduced(8),
            Deduced(4),
            Deduced(7),
            Given(5),
            Given(9),
            Deduced(3),
            Deduced(1),
            Given(2),
            Deduced(6)
          ),
          Array(
            Given(6),
            Deduced(5),
            Deduced(3),
            Given(7),
            Deduced(1),
            Deduced(2),
            Deduced(9),
            Deduced(4),
            Deduced(8)
          )
        )
      )
      show(Sudoku.solved(problem)) ==> show(solution)
    }
  }
}
