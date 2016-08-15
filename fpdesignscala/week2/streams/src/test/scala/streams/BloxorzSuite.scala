package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
      * This method applies a list of moves `ls` to the block at position
      * `startPos`. This can be used to verify if a certain list of moves
      * is a valid solution, i.e. leads to the goal.
      */
    def solve(ls: List[Move]): Block =
    ls.foldLeft(startBlock) { case (block, move) => move match {
      case Left => block.left
      case Right => block.right
      case Up => block.up
      case Down => block.down
    }
    }
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }


  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(terrain(Pos(1, 1)), "1,1") // start
      assert(terrain(Pos(4, 7)), "4,7") // goal
      assert(terrain(Pos(5, 8)), "5,8")
      assert(!terrain(Pos(5, 9)), "5,9")
      assert(terrain(Pos(4, 9)), "4,9")
      assert(!terrain(Pos(6, 8)), "6,8")
      assert(!terrain(Pos(4, 11)), "4,11")
      assert(!terrain(Pos(-1, 0)), "-1,0")
      assert(!terrain(Pos(0, -1)), "0,-1")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1, 1))
    }
  }


  test("startBlock legalNeighbors") {
    new Level1 {
      assert(startBlock.legalNeighbors === List((startBlock.right, Right), (startBlock.down, Down)))
    }
  }

  test("startBlock.right legalNeighbors") {
    new Level1 {
      val right = startBlock.right

      assert(right.legalNeighbors === List((startBlock, Left), (right.right, Right), (right.down, Down)))
    }
  }

  test("startBlock.down legalNeighbors") {
    new Level1 {
      val down = startBlock.down

      assert(down.legalNeighbors === List((down.right, Right), (down.up, Up)))
    }
  }

  test("neighborsWithHistory") {
    new Level1 {
      assert(neighborsWithHistory(startBlock, history = List(Left, Up)).toSet
        === Set((startBlock.right, List(Right, Left, Up)), (startBlock.down, List(Down, Left, Up))))
    }
  }

  test("newNeighborsOnly") {
    new Level1 {
      val neighbors = Set((startBlock.right, List(Right, Left, Up)), (startBlock.down, List(Down, Left, Up))).toStream

      assert(newNeighborsOnly(neighbors, Set(startBlock.right, startBlock.left)).toSet
        === Set((startBlock.down, List(Down, Left, Up))))
    }
  }



  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }


  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

}
