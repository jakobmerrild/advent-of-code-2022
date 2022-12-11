package days

import scala.util.matching.Regex

final case class Day7(input: String) extends Puzzle {
  import Day7._

  val changeDirectoryPattern = """^\$ cd (.*)$""".r
  val listPattern = """^\$ ls$""".r
  val filePattern = """^(\d+) (.*)$""".r
  val dirPattern = """^dir (.*)$""".r

  def buildList(results: List[ListResult], lines: List[String]): (List[ListResult], List[String]) =
    lines match
      case head :: next =>
        head match
          case filePattern(size, name) => buildList(ListFile(name, size.toInt) :: results, next)
          case dirPattern(name)        => buildList(ListDir(name) :: results, next)
          case _                       => (results, lines)
      case Nil => (results, Nil)

  def parse(state: ParseState): ParseState =
    state.remainingLines match
      case head :: next =>
        head match
          case changeDirectoryPattern(dirName) =>
            parse(ParseState(next, Cd(dirName) :: state.instructions))
          case listPattern() =>
            val (listResults, remainingLines) = buildList(Nil, next)
            parse(ParseState(remainingLines, Ls(listResults) :: state.instructions))
      case Nil => state

  lazy val parsedInput: ParseState = parse(ParseState(input.split("\n").toList, Nil))

  def performInstruction(fs: FileSystem, instruction: Instruction): FileSystem =
    instruction match
      case Ls(contents) =>
        val contentsToPaths = contents.map {
          _ match
            case ListFile(name, size) => File(name, size, Some(fs.currentPath))
            case ListDir(name)        => new Directory(name)
        }.toSet
        fs.currentPath.withChildren(contentsToPaths)
        fs
      case Cd(dirName) =>
        dirName match
          case ".." =>
            fs.copy(currentPath = fs.currentPath.parent.get)
          case x =>
            val newCurrentPath = fs.currentPath.children.collectFirst {
              case d: Directory if d.name == x => d
            }.get
            fs.copy(currentPath = newCurrentPath)

  def addSmallPaths(acc: List[Path], path: Path): List[Path] =
    path match
      case File(name, size, parent) => acc
      case d: Directory =>
        if (d.totalSize <= 100_000)
          d.children.foldLeft(d :: acc)(addSmallPaths)
        else d.children.foldLeft(acc)(addSmallPaths)

  lazy val root: Directory = {
    val rootDir = new Directory("/")
    val fileSystem = FileSystem(rootDir, rootDir)
    parsedInput.instructions.reverse
      .drop(1)
      .foldLeft(fileSystem)(performInstruction)
      .rootPath
  }
  def allDirectories: List[Directory] =
    def collect(acc: List[Directory], p: Path): List[Directory] =
      p match
        case File(name, size, parent) => acc
        case d: Directory             => d :: d.children.foldLeft(acc)(collect)
    collect(Nil, root)

  override def solve: String = {
    val pathsSmallEnough = addSmallPaths(Nil, root)
    pathsSmallEnough.map(_.totalSize).sum.toString()
  }

  override def solve2: String = {
    val systemSize = 70_000_000
    val desiredFreeSpace = 30_000_000
    val totalSpaceCurrentLy = root.totalSize
    val unusedSpace = systemSize - totalSpaceCurrentLy
    val toDelete = desiredFreeSpace - unusedSpace

    allDirectories.sortBy(_.totalSize).dropWhile(_.totalSize < toDelete).head.totalSize.toString
  }
}

object Day7 {
  sealed trait Path {
    def name: String
    def size: Int
    def children: Set[Path]
    def totalSize: Int = size + children.map(_.totalSize).sum
    def parent: Option[Directory]
    def absolute: String = s"${parent.map(p => p.absolute + "/").getOrElse("")}$name"
  }

  final case class File(name: String, size: Int, parent: Option[Directory]) extends Path {
    override val children = Set.empty
  }

  final class Directory(val name: String) extends Path {
    override val size = 0

    var parent: Option[Directory] = None

    var children: Set[Path] = Set.empty

    def withParent(parent: Directory): Unit = this.parent = Some(parent)

    def withChildren(newChildren: Set[Path]): Unit =
      newChildren.collect { case d: Directory => d }.foreach(_.withParent(this))
      this.children = this.children union newChildren
  }

  final case class FileSystem(rootPath: Directory, currentPath: Directory)

  sealed trait ListResult
  final case class ListFile(name: String, size: Int) extends ListResult
  final case class ListDir(name: String) extends ListResult

  sealed trait Instruction
  final case class Ls(contents: List[ListResult]) extends Instruction
  final case class Cd(dirName: String) extends Instruction

  final case class ParseState(remainingLines: List[String], instructions: List[Instruction])
}
