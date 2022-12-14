package aoc

import scala.collection.mutable.{Map, Queue, ArrayBuffer}

object day11 extends App:
  def parseVariable(variable: String, default: Int): Int =
    if (variable == "old") default else variable.toInt

  def parseOperation(input: String): Int => Int =
    input.trim match
      case s"$a + $b" =>
        (x: Int) => parseVariable(a, x) + parseVariable(b, x)
      case s"$a * $b" =>
        (x: Int) => parseVariable(a, x) * parseVariable(b, x)

  def parseTest(input: String): Int => Boolean =
    input.trim match
      case s"$rest by $div" => (x: Int) => x % div.toInt == 0

  def parseInstruction(input: String): Int =
    input.trim match
      case s"$rest monkey $i" => i.toInt

  class Monkey(
      val items: Queue[Int],
      operation: Int => Int,
      test: Int => Boolean,
      ifTrue: Int,
      ifFalse: Int
  ):
    def decideTarget(item: Int): (Int, Int) =
      val newWorryLevel = operation(item) / 3
      val target =
        if (test(newWorryLevel))
          ifTrue
        else
          ifFalse

      (newWorryLevel, target)

  def parseInput(inputPath: String) =
    getFileContent(inputPath).grouped(7).map { data =>
      val startingItems =
        data(1).split(":").last.split(", ").map(_.trim.toInt)
      val operation = parseOperation(data(2).split("=").last)
      val test = parseTest(data(3))
      val ifTrue = parseInstruction(data(4))
      val ifFalse = parseInstruction(data(5))

      new Monkey(Queue.from(startingItems), operation, test, ifTrue, ifFalse)
    }

  def runRound(monkeys: Array[Monkey], inspects: ArrayBuffer[Int]) =
    monkeys.zipWithIndex.foreach { case (monkey, index) =>
      for (_ <- 1 to monkey.items.length)
        val item = monkey.items.dequeue()
        val (newWorryLevel, target) = monkey.decideTarget(item)
        // println(s"$index: item $item -> $newWorryLevel to $target")
        monkeys(target).items.enqueue(newWorryLevel)
        inspects(index) += 1
    }
    // monkeys.map(_.items.toList).zipWithIndex.foreach(println)
    // println(inspects)
    // println("=" * 10)

  def solve(monkeys: Array[Monkey], rounds: Int) =
    val inspects = ArrayBuffer.fill(monkeys.length)(0)
    for (_ <- 1 to rounds)
      runRound(monkeys, inspects)
    val monkeyBusiness = inspects.sorted(Ordering.Int.reverse).take(2).product
    monkeyBusiness

  val monkeys = parseInput("./inputs/day11/data.txt").toArray
  val inspects = solve(monkeys, 20)
  println(inspects)
