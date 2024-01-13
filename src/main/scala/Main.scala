object Main extends App{
  println("Start game")
  Stty.bufferByCharacter()
  Game.start(25)
  println("End game")
}