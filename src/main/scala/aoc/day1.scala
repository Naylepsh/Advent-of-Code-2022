package aoc

import scala.collection.mutable

def solve(inputPath: String, topCount: Int) = {
  var currentStock = 0
  var stocks = mutable.ArrayBuffer[Int]()

  getFileContent(inputPath).foreach { row =>
    if (row == "") {
      stocks.addOne(currentStock)
      currentStock = 0
    } else {
      currentStock += row.toInt
    }
  }

  val sorted = stocks.zipWithIndex.sortBy { case (stocks, owner) =>
    stocks
  }(Ordering.Int.reverse)

  sorted.take(topCount)
}

def solveInit(inputPath: String, topCount: Int) = {
  val topCarriers = solve(inputPath, topCount)

  println(topCarriers)
}

def solveActual(inputPath: String, topCount: Int) = {
  val topCarriers = solve(inputPath, topCount)

  println(topCarriers.map(_._1).sum)
}

@main
def run(): Unit =
  // solveInit("./inputs/day1/data.txt", 3)
  solveActual("./inputs/day1/data.txt", 3)
