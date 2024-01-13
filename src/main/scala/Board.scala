import scala.annotation.meta.companionObject

class Board(val size: Int, val cells: List[Cell]):
  val random = scala.util.Random

  def snake = cells
    .filter(_.isInstanceOf[Head])
    .map(_.asInstanceOf[Head])
    .headOption

  def withSnake(snake: Head): Board = {
    val newCells = cells.map {
      case Empty(position) if snake.position == position =>
        snake
      case Food(position) if snake.position == position =>
        snake.copy(length = snake.length + 1)
      case cell => cell
    }
    new Board(size, newCells)
  }

  def withSnakePart(snakePart: Tail): Board = {
    val newCells = cells.map {
      case Empty(position) if snakePart.position == position =>
        snakePart
      case cell => cell
    }
    new Board(size, newCells)
  }

  def withFood: Board = {
    val legalCells = cells.filter { 
      case Empty((x, y)) => true 
      case _ => false
    }.toList
    val food = Food(legalCells(random.nextInt(legalCells.length)).position)
    val newCells = cells.map {
      case Empty(position) if food.position == position =>
        food
      case cell => cell
    }
    new Board(size, newCells)
  }

  def withoutFood: Board = {
    val newCells = cells.map {
      case Food(position) =>
        Empty(position)
      case cell => cell
    }
    new Board(size, newCells)
  }

  def updated(direction: Direction): Board =
    val newCells = cells.map {
      case Head(position, length, direction) => Tail(position, length)
      case Tail(position, timeToLive) if timeToLive > 1 =>
        Tail(position, timeToLive - 1)
      case Tail(position, _) => Empty(position)
      case cell              => cell
    }
    val newHead = nextHead(
      snake
        .map(_.copy(direction = direction))
        .getOrElse(Head((-1, -1), 0, Direction.UP))
    )
    if (foodPresented(newCells)) then
      new Board(size, newCells).withSnake(newHead)
    else new Board(size, newCells).withSnake(newHead).withFood

  private def nextHead(snake: Head): Head =
    val (x, y) = snake.position
    snake.direction match
      case Direction.UP    => snake.copy(position = (x - 1, y))
      case Direction.DOWN  => snake.copy(position = (x + 1, y))
      case Direction.LEFT  => snake.copy(position = (x, y - 1))
      case Direction.RIGHT => snake.copy(position = (x, y + 1))

  def valid: Boolean = snake.map(_ => true).getOrElse(false)

  private def foodPresented(cells: List[Cell]): Boolean =
    cells.exists(_.isInstanceOf[Food])

object Board:
  def empty(size: Int): Board =
    val cells = for
      x <- 0 to size - 1
      y <- 0 to size - 1
    yield Empty((x, y))
    val cellsWithWalls = cells.map {
      case Empty((x, y))
          if x == 0 || x == size - 1 || y == 0 || y == size - 1 =>
        Wall((x, y))
      case cell => cell
    }.toList
    new Board(size, cellsWithWalls)
      .withSnake(Head((size / 2, size / 2), 2, Direction.RIGHT))
