package days

final case class Day12(input: String) extends Puzzle {
  import Day12._

  private def readNode(c: Char, coordinates: Coordinates): Node =
    if (c == 'S')
      Node(coordinates, 'a'.toInt - 97)
    else if (c == 'E')
      Node(coordinates, 'z'.toInt - 97)
    else
      Node(coordinates, c.toInt - 97)

  lazy val parsedInput: State =
    val nodes = input
      .split("\n")
      .zipWithIndex
      .flatMap { case (row, y) =>
        row.zipWithIndex.map { case (column, x) =>
          readNode(column, Coordinates(x, y))
        }
      }

    val nodeMap = nodes.map(n => (n.coordinates -> n)).toMap
    val edges = nodes.flatMap { node =>
      val maybeEdges = List(
        node.coordinates.copy(x = node.coordinates.x - 1),
        node.coordinates.copy(x = node.coordinates.x + 1),
        node.coordinates.copy(y = node.coordinates.y - 1),
        node.coordinates.copy(y = node.coordinates.y - 1)
      ).map { possibleCoordinate =>
        val otherNodeOpt =
          nodeMap.get(possibleCoordinate)
        otherNodeOpt.flatMap { otherNode =>
          if (otherNode.elevation <= node.elevation + 1)
            Some(Edge(node, otherNode))
          else
            None
        }
      }
      maybeEdges.flatten
    }

    val startCoordinates = input
      .split("\n")
      .zipWithIndex
      .flatMap { case (row, y) =>
        row.zipWithIndex.flatMap { case (c, x) => if (c == 'S') Some(Coordinates(x, y)) else None }
      }
      .head

    val endCoordinates = input
      .split("\n")
      .zipWithIndex
      .flatMap { case (row, y) =>
        row.zipWithIndex.flatMap { case (c, x) => if (c == 'E') Some(Coordinates(x, y)) else None }
      }
      .head

    State(startCoordinates, endCoordinates, Graph(nodes.toSet, edges.toSet))

  override def solve: String = parsedInput.toString()
  override def solve2: String = "???"
}

object Day12 {
  final case class Coordinates(x: Int, y: Int)
  final case class Node(coordinates: Coordinates, elevation: Int)
  final case class Edge(from: Node, to: Node) {
    def isValid: Boolean =
      from.elevation + 1 >= to.elevation
  }
  final case class Graph(nodes: Set[Node], edges: Set[Edge])

  final case class State(startCoordinates: Coordinates, endCoordinates: Coordinates, graph: Graph)
}
