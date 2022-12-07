package aoc

object day2 extends App:
  enum Shape:
    case Rock, Paper, Scissors

  def asOpponentShape(str: String): Shape = str match
    case "A" => Shape.Rock
    case "B" => Shape.Paper
    case "C" => Shape.Scissors

  def shapeScore(shape: Shape): Int = shape match
    case Shape.Rock     => 1
    case Shape.Paper    => 2
    case Shape.Scissors => 3

  enum GameResult:
    case Win, Lose, Draw

  def resultScore(result: GameResult): Int = result match
    case GameResult.Win  => 6
    case GameResult.Draw => 3
    case GameResult.Lose => 0

  object init:
    private def asPlayerShape(str: String): Shape = str match
      case "X" => Shape.Rock
      case "Y" => Shape.Paper
      case "Z" => Shape.Scissors

    private def calculateOutcome(opponent: Shape, player: Shape): Int =
      val result =
        (opponent, player) match
          case (Shape.Rock, Shape.Paper)     => GameResult.Win
          case (Shape.Rock, Shape.Scissors)  => GameResult.Lose
          case (Shape.Paper, Shape.Rock)     => GameResult.Lose
          case (Shape.Paper, Shape.Scissors) => GameResult.Win
          case (Shape.Scissors, Shape.Rock)  => GameResult.Win
          case (Shape.Scissors, Shape.Paper) => GameResult.Lose
          case (_, _)                        => GameResult.Draw

      resultScore(result) + shapeScore(player)

    private def runGame(turns: List[(Shape, Shape)]): List[Int] =
      turns.map { case (opponent, player) =>
        calculateOutcome(opponent, player)
      }

    def parseGame(str: String): (Shape, Shape) =
      str.split(" ").toList match
        case opponent :: player :: Nil =>
          (asOpponentShape(opponent), asPlayerShape(player))

    def solve(inputPath: String) =
      val game =
        runGame(getFileContent(inputPath).toList.map(parseGame))
      game.sum

  object actual:
    private def asGameResult(str: String): GameResult = str match
      case "X" => GameResult.Lose
      case "Y" => GameResult.Draw
      case "Z" => GameResult.Win

    private def calculateShape(opponent: Shape, result: GameResult): Shape =
      (opponent, result) match
        case (shape, GameResult.Draw)          => shape
        case (Shape.Rock, GameResult.Lose)     => Shape.Scissors
        case (Shape.Rock, GameResult.Win)      => Shape.Paper
        case (Shape.Paper, GameResult.Lose)    => Shape.Rock
        case (Shape.Paper, GameResult.Win)     => Shape.Scissors
        case (Shape.Scissors, GameResult.Lose) => Shape.Paper
        case (Shape.Scissors, GameResult.Win)  => Shape.Rock

    private def calculateOutcome(player: Shape, result: GameResult) =
      resultScore(result) + shapeScore(player)

    def parseGame(str: String): (Shape, GameResult) =
      str.split(" ").toList match
        case opponent :: gameResult :: Nil =>
          (asOpponentShape(opponent), asGameResult(gameResult))

    def solve(inputPath: String) =
      getFileContent(inputPath).toList
        .map(parseGame)
        .map { case (opponent, result) =>
          calculateOutcome(calculateShape(opponent, result), result)
        }
        .sum

  // val res = day2.init.solve("./inputs/day2/data.txt")
  val res = day2.actual.solve("./inputs/day2/data.txt")
  println(res)
