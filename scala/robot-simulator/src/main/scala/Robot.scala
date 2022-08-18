import cats.Foldable
import cats.Traverse
import cats.implicits.*

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

enum Bearing:
  case North, East, South, West

case class Robot(bearing: Bearing, coordinates: (Int, Int)) {
  def turnRight: Robot = bearing match {
    case Bearing.North => Robot(Bearing.East, coordinates)
    case Bearing.East => Robot(Bearing.South, coordinates)
    case Bearing.South => Robot(Bearing.West, coordinates)
    case Bearing.West => Robot(Bearing.North, coordinates)
  }

  def turnLeft: Robot = bearing match {
    case Bearing.North => Robot(Bearing.West, coordinates)
    case Bearing.East => Robot(Bearing.North, coordinates)
    case Bearing.South => Robot(Bearing.East, coordinates)
    case Bearing.West => Robot(Bearing.South, coordinates)
  }

  def advance: Robot = bearing match {
    case Bearing.North => Robot(bearing, (coordinates._1, coordinates._2 + 1))
    case Bearing.East => Robot(bearing, (coordinates._1 + 1, coordinates._2))
    case Bearing.South => Robot(bearing, (coordinates._1, coordinates._2 - 1))
    case Bearing.West => Robot(bearing, (coordinates._1 - 1, coordinates._2))
  }

  def genericSimulate[F[_]: Foldable](simulation: F[Char]): Robot = simulation.foldLeft(this) { case (current, action) =>
    action match {
      case 'L' => current.turnLeft
      case 'R' => current.turnRight
      case 'A' => current.advance
    }
  }

  def +(value: Robot): Robot = value.bearing match {
    case Bearing.East => this.turnLeft
    case Bearing.West => this.turnRight
    case Bearing.North => this.copy(coordinates = (value.coordinates._1 + coordinates._1, value.coordinates._2 + coordinates._2))
    case _ => throw new RuntimeException("not possible, we always start from North")
  }

  def singleSimulate(simulation: Char)(implicit ec: ExecutionContext): Future[Robot] =
    simulation match {
      case 'L' => Future(turnLeft)
      case 'R' => Future(turnRight)
      case 'A' => Future(advance)
  }

  def simulate(simulation: String)(implicit ec: ExecutionContext): Robot = {
    val result = for {
      robots <- Traverse[List].traverse(simulation.toList)(singleSimulate)
    } yield {
      robots.foldLeft(this)(_ + _)
    }

    Await.result(result, 1.second)
  }
}