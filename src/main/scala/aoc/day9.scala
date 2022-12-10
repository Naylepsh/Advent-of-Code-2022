package aoc

import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

object day9 extends App:
  extension [A](xs: List[A])
    def cartesian = for
      x <- xs
      y <- xs
    yield (x, y)

  type Position = (Int, Int)
  object Positions:
    def areWithinReach(a: Position, b: Position): Boolean =
      List(-1, 0, 1).cartesian.find { case (dx, dy) =>
        (a._1 + dx, a._2 + dy) == b
      }.isDefined

  enum Direction:
    case Right, Up, Left, Down

  extension (positions: Set[Position])
    def toString(xMax: Int, yMax: Int, head: Position): String =
      val map = ArrayBuffer.from(
        Array.ofDim[String](yMax, xMax).map(_.map(_ => "."))
      )

      positions.foreach { case (x, y) =>
        map(yMax - y - 1)(x) = "#"
      }

      map(yMax - head._2 - 1)(head._1) = "H"

      map.zipWithIndex
        .map { case (row, index) =>
          s"""${yMax - index - 1}: ${row.mkString("")}"""
        }
        .mkString("\n")

  def solveA(inputPath: String): Int =
    var headPosition = (0, 0)
    var tailPosition = (0, 0)
    val positions = Set[Position](tailPosition)

    def moveBy(dx: Int, dy: Int): Unit =
      val (hx, hy) = headPosition

      val newHeadPosition = (hx + dx, hy + dy)
      if (!Positions.areWithinReach(newHeadPosition, tailPosition))
        // println(
        //   s"head: ${headPosition} -> ${newHeadPosition}; tail: $tailPosition -> $headPosition"
        // )
        tailPosition = headPosition
        positions.add(tailPosition)

      headPosition = newHeadPosition

    def move(direction: Direction, steps: Int): Unit =
      val (dx, dy) = direction match
        case Direction.Right => (1, 0)
        case Direction.Up    => (0, 1)
        case Direction.Left  => (-1, 0)
        case Direction.Down  => (0, -1)

      1 to steps foreach { _ =>
        moveBy(dx, dy)
      }

    getFileContent(inputPath).foreach { input =>
      // println(positions.toString(6, 5, headPosition))
      // println("=" * 9)

      input match
        case s"R $steps" => move(Direction.Right, steps.toInt)
        case s"U $steps" => move(Direction.Up, steps.toInt)
        case s"L $steps" => move(Direction.Left, steps.toInt)
        case s"D $steps" => move(Direction.Down, steps.toInt)

    }

    // println(positions.toList)
    positions.size

  println(solveA("./inputs/day9/data.txt"))
