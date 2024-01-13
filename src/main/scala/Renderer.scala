import com.googlecode.lanterna.terminal.DefaultTerminalFactory
import com.googlecode.lanterna.screen.TerminalScreen

object Renderer {
  val terminal = DefaultTerminalFactory().createTerminal()
  val screen = TerminalScreen(terminal)

  screen.startScreen()

  def draw(board: Board) =
    val textGraphics = screen.newTextGraphics()
    val matrix = Array.fill(board.size, board.size)('.')
    board.cells.foreach {
      case Empty((x, y))      => matrix(x)(y) = '.'
      case Food((x, y))       => matrix(x)(y) = '@'
      case Head((x, y), _, _) => matrix(x)(y) = '$'
      case Tail((x, y), _)    => matrix(x)(y) = '%'
      case Wall((x, y))       => matrix(x)(y) = '#'
      case null               => // Do nothing
    }
    for (row <- matrix) {
      textGraphics.putString(0, matrix.indexOf(row), row.mkString(" "))
    }
    screen.refresh()

  def stop =
    screen.newTextGraphics().putString(0, 0, "game over")
    screen.stopScreen()
    terminal.close()
}
