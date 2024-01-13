import java.io.BufferedReader
import java.io.InputStreamReader
import java.util.concurrent.ConcurrentLinkedQueue

object Game:
  val renderer = Renderer
  val random = scala.util.Random
  val keyQueue = ConcurrentLinkedQueue[Direction]()
  @volatile var continueListen = true

  def start(size: Int): Unit =
    var board = Board.empty(size).withFood
    val kbListener = keyboardListener
    kbListener.start()
    loop(board)


  private def loop(board: Board): Unit =
    renderer.draw(board)
    Thread.sleep(250)
    val direction = Option(keyQueue.poll())
      .getOrElse(
        board.snake.getOrElse(Head((0, 0), 0, Direction.RIGHT)).direction
      )
    val newBoard = board.updated(direction)
    if newBoard.valid then loop(newBoard)
      else renderer.stop

  private def keyboardListener: Thread =
    new Thread(() => {
      val reader = BufferedReader(InputStreamReader(System.in))
      while (continueListen)
        reader.read() match
          case 'w' => keyQueue.add(Direction.UP)
          case 'a' => keyQueue.add(Direction.LEFT)
          case 's' => keyQueue.add(Direction.DOWN)
          case 'd' => keyQueue.add(Direction.RIGHT)
          case 'q' => continueListen = false
          case _   => // Do nothing    
    })
