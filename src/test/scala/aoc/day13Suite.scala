package aoc

import io.circe._
import io.circe.parser._
import day13._

class day13Suite extends munit.FunSuite:
  import day13Suite._

  test("parsing simple list") {
    val result = execute("[1,1,3,1,1]", "[1,1,5,1,1]")

    assertEquals(result, Comparison.Lower)
  }

  test("parsing nested lists") {
    val result = execute("[[1],[2,3,4]]", "[[1],4]")

    assertEquals(result, Comparison.Lower)
  }

  test("parsing simple list and nested one") {
    val result = execute("[9]", "[[8,7,6]]")

    assertEquals(result, Comparison.Greater)
  }

  test("parsing right list being longer") {
    val result = execute("[[4,4],4,4]", "[[4,4],4,4,4]")

    assertEquals(result, Comparison.Lower)
  }

  test("parsing left list being longer") {
    val result = execute("[7,7,7,7]", "[7,7,7]")

    assertEquals(result, Comparison.Greater)
  }

  test("parsing empty list") {
    val result = execute("[]", "[3]")

    assertEquals(result, Comparison.Lower)
  }

  test("parsing nested empty lists") {
    val result = execute("[[[]]]", "[[]]")

    assertEquals(result, Comparison.Greater)
  }

  test("parsing deeply nested lists") {
    val result =
      execute("[1,[2,[3,[4,[5,6,7]]]],8,9]", "[1,[2,[3,[4,[5,6,0]]]],8,9]")

    assertEquals(result, Comparison.Greater)
  }

object day13Suite:
  def execute(a: String, b: String): Comparison =
    (parse(a), parse(b)) match
      case (Right(left), Right(right)) => compare(left, right)
