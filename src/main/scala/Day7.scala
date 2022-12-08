
import scala.collection.mutable.{ListBuffer, Map}
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

trait FSEntry
case class File(name: String, size: Long) extends FSEntry
case class Dir(name: String, contentFiles: Map[String, File], contentDirs: Map[String, Dir], parent: Option[Dir]) extends FSEntry {
  override def toString: String = s"Directory '${name}'"
}
object Day7 extends RegexParsers{
  var topDir: Dir = _
  var currentDir: Option[Dir] = None
  def name = """([a-z.A-Z/]+)""".r ^^{name => name}
  def number = """(\d+)""".r ^^ {n => java.lang.Long.parseLong(n)}
  def cdLine = "$ cd " ~> name ^^ { name =>
    if(name != "..") {
      currentDir = currentDir match {
        case Some(d) => Some(d.contentDirs(name))
        case None =>
          topDir = Dir(name, Map(), Map(), None)
          //println(s"Set top dir to ${topDir.name}")
          Some(topDir)
      }
    } else { //go one up
      currentDir = currentDir.get.parent
    }
  }

  def dirStatement = """dir """ ~> name ^^ { dirname =>
    //println(s"Add dir $dirname to ${currentDir.get.name}")
    currentDir.get.contentDirs.getOrElseUpdate(dirname, Dir(dirname, Map(), Map(), currentDir))
  }
  def fileStament =  number ~ name ^^ { case size ~ filename =>
    //println(s"Add file $filename to ${currentDir.get.name}")
    currentDir.get.contentFiles(filename) = File(filename, size)
  }

  def lsLine = "$ ls" //^^{_ => println("ls line")}

  def file: Parser[FSEntry] = rep(cdLine | lsLine | fileStament | dirStatement) ^^ { _ => topDir}

  def parseFile(name: String): FSEntry = parse(file, Source.fromResource(name).reader()) match {
    case Success(matched, _) => matched
    case Failure(msg, _) => throw Exception(s"FAILURE: $msg")
    case Error(msg, _) => throw Exception(s"ERROR: $msg")
  }

  def visit(fe :FSEntry, level: Int=0): Unit =
    fe match
      case file: File => println((0 to level).map(_ => "\t").mkString + file.toString)
      case folder: Dir =>
        println((0 to level).map(_ => "\t").mkString + s"DIR ${folder.name}")
        folder.contentFiles.foreach((_, file) => visit(file, level + 1))
        folder.contentDirs.foreach((_, dir) => visit(dir, level + 1))

  var acc: Long = 0

  var smallest: Long = 10000000000000l

  def calcDirSize(fs: Dir): Long = {
    val fileSize = fs.contentFiles.values.map(_.size).sum
    val subSizes = fs.contentDirs.map((_, d) => calcDirSize(d)).sum
    val totalSize = fileSize + subSizes

    if (totalSize < 100000) {
      acc += totalSize
    }

    if (totalSize > (30000000-25195167) && totalSize < smallest) {
      println(s"updating smallest to $totalSize")
      smallest = totalSize
    }

    return  totalSize
  }

  def example: Long = {
    acc = 0
    val top = parseFile("day7/example.txt").asInstanceOf[Dir]
    calcDirSize(top)
    return acc
  }

  def star1: Long = {
    acc = 0
    val top = parseFile("day7/star1.txt").asInstanceOf[Dir]
    calcDirSize(top)
    return acc
  }

  def star2: Long = {
    smallest = 10000000000000l
    val needed = 70000000 - 44804833
    println(s"needed is $needed")
    val top = parseFile("day7/star1.txt").asInstanceOf[Dir]
    println("FUll size is " +calcDirSize(top))
    return smallest
  }

}



