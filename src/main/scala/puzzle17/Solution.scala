package puzzle17

import scala.io.Source
import scala.util.Try

object int {
  def unapply(str: String): Option[Int] = str.toIntOption
}

trait Element {
  val neighbors: Set[_<:Element]
}

case class Cube(x: Int,
                y: Int,
                z: Int
               ) extends Element {
  lazy val neighbors: Set[Cube] = Cube.getNeighbors(this)
}

object Cube {
  def getNeighbors(cube: Cube): Set[Cube] = {
    val disp = Seq(-1, 0, 1)
    val xs = disp.map { d =>
      cube.copy(x = cube.x + d)
    }
    val ys = xs.flatMap { c =>
      disp.map { d =>
        c.copy(y = c.y + d)
      }
    }
    val set = ys.flatMap { c =>
      disp.map { d =>
        c.copy(z = c.z + d)
      }
    }.toSet.diff(Set(cube))
    set
  }
}

case class HyperCube(x: Int,
                     y: Int,
                     z: Int,
                     w: Int
               ) extends Element {
  lazy val neighbors: Set[HyperCube] = HyperCube.getNeighbors(this)
}

object HyperCube {
  def getNeighbors(cube: HyperCube): Set[HyperCube] = {
    val disp = Seq(-1, 0, 1)
    val xs = disp.map { d =>
      cube.copy(x = cube.x + d)
    }
    val ys = xs.flatMap { c =>
      disp.map { d =>
        c.copy(y = c.y + d)
      }
    }
    val zs = ys.flatMap { c =>
      disp.map { d =>
        c.copy(z = c.z + d)
      }
    }
    val set = zs.flatMap { c =>
      disp.map { d =>
        c.copy(w = c.w + d)
      }
    }.toSet.diff(Set(cube))
    set
  }
}

object Solution {

  val input: IndexedSeq[String] = Source.fromResource("input17.txt")
    .getLines
    .toIndexedSeq

  def step(active: Set[Element], end: Int): Set[Element] = {
    if (end == 0) active
    else {
      val map = active.foldLeft(Map.empty[Element, Int]) {
        case (activeCount, cube) =>
          cube.neighbors.foldLeft(activeCount) {
            case (map, neighbor) =>
              map ++ Map(neighbor -> (map.getOrElse(neighbor, 0) + 1))
          }
      }
      val newActive = map.foldLeft(Set.empty[Element]) {
        case (newActive, (cube, count)) =>
          if (active.contains(cube) && (count == 2 || count == 3)) newActive ++ Set(cube)
          else if (!active.contains(cube) && count == 3) newActive ++ Set(cube)
          else newActive
      }
      step(newActive, end - 1)
    }
  }

  def partOne: Int = {
    val active = input.flatMap(_.toCharArray.toSeq).zipWithIndex.foldLeft(Set.empty[Element]) {
      case (active, (c, idx)) if c == '#' =>
        val x = idx % input.head.length
        val y = idx / input.head.length
        active ++ Set(Cube(x, y, 0))
      case (active, _) => active
    }
    val state = step(active, 6)
    state.size
  }

  def partTwo: BigInt = {
    val active = input.flatMap(_.toCharArray.toSeq).zipWithIndex.foldLeft(Set.empty[Element]) {
      case (active, (c, idx)) if c == '#' =>
        val x = idx % input.head.length
        val y = idx / input.head.length
        active ++ Set(HyperCube(x, y, 0, 0))
      case (active, _) => active
    }
    val state = step(active, 6)
    state.size
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}