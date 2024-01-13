sealed trait Cell(val position: (Int, Int))

final case class Empty(override val position: (Int, Int)) extends Cell(position)

final case class Food(override val position: (Int, Int)) extends Cell(position)

final case class Head(override val position: (Int, Int), length: Int, direction: Direction) extends Cell(position)

final case class Tail(override val position: (Int, Int), timeToLive: Int) extends Cell(position)

final case class Wall(override val position: (Int, Int)) extends Cell(position)