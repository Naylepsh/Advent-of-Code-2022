package aoc

object day14 extends App:
  enum Atom:
    case Rock, Sand, Air
  object Atom:
    extension (atom: Atom)
      def show: String = atom match
        case Rock => "#"
        case Sand => "o"
        case Air  => "."

      def isSolid: Boolean = atom match
        case Rock | Sand => true
        case _           => false

  type Cave = Array[Array[Atom]]
  object Cave:
    def apply(width: Int, height: Int): Cave =
      (0 to width)
        .map: _ =>
          (0 to height)
            .map: _ =>
              Atom.Air
            .toArray
        .toArray

    extension (cave: Cave)
      def addRockFormation(traces: List[Trace]): Unit =
        traces
          .zip(traces.tail)
          .foreach: (from, to) =>
            if from.x == to.x
            then
              (from.y.min(to.y).to(from.y.max(to.y))).foreach: h =>
                cave(h)(from.x) = Atom.Rock
            else
              (from.x.min(to.x).to(from.x.max(to.x))).foreach: w =>
                cave(from.y)(w) = Atom.Rock

      private def contains(x: Int, y: Int): Boolean =
        0 <= y && y < cave.length && 0 <= x && x < cave.head.length

      def takeOf(x: Int, y: Int): Option[Atom] =
        if 0 <= y && y < cave.length && 0 <= x && x < cave.head.length
        then Some(cave(y)(x))
        else None

      def addAt(x: Int, y: Int, atom: Atom): Unit =
        if 0 <= y && y < cave.length && 0 <= x && x < cave.head.length
        then cave(y)(x) = atom

      @annotation.tailrec
      def trickleSand(fromX: Int, fromY: Int): Boolean =
        val nextPosition = List(
          (fromX, fromY + 1),
          (fromX - 1, fromY + 1),
          (fromX + 1, fromY + 1)
        ).find: (x, y) =>
          cave.takeOf(x, y).map(_.isSolid) == Some(false)
        nextPosition match
          case None =>
            takeOf(fromX - 1, fromY + 1)
              .map: _ =>
                cave.addAt(fromX, fromY, Atom.Sand)
              .isEmpty
          case Some(x, y) => trickleSand(x, y)

      @annotation.tailrec
      def trickleSandUntilSpills(
          fromX: Int,
          fromY: Int,
          sandUnitsPlaced: Int = 0
      ): Int =
        if cave.trickleSand(fromX, fromY) then sandUnitsPlaced
        else trickleSandUntilSpills(fromX, fromY, sandUnitsPlaced + 1)

      def show(skipX: Int, limitX: Int, skipY: Int, limitY: Int): String =
        cave
          .drop(skipX)
          .take(limitX)
          .map: line =>
            line.drop(skipY).take(limitY).map(_.show).mkString("")
          .mkString("\n")

  case class Trace(x: Int, y: Int)
  object Trace:
    def fromLine(line: String): List[Trace] =
      line
        .split(" -> ")
        .map: rawTrace =>
          rawTrace.split(",") match
            case Array(x, y) => Trace(x.toInt, y.toInt)
        .toList

  import Cave.*

  val cave = Cave(200, 600)
  getFileContent("./inputs/day14/data.txt")
    .map: line =>
      Trace.fromLine(line)
    .foreach: traces =>
      cave.addRockFormation(traces)
  val units = cave.trickleSandUntilSpills(500, 0)
  println(units)
    // val result = cave.trickleSand(500, 0)
    // println(s"$i :: $result")
    // println(cave.show(0, 10, 494, 10))
