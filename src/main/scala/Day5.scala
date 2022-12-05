import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

case class Instruction(count: Int, source: Int, target: Int)

object Day5:
  def parseStacks(lines: Iterator[String]): Map[Int, mutable.Stack[Char]] = {
    val separator: Char = ' '
    val itemDescription = "\\[([A-Z)])\\] ".r
    val result = mutable.Map[Int, mutable.Stack[Char]]()
    for(line <- lines) {
      line.appended(separator)
        .sliding(4,4)
        .zipWithIndex
        .foreach {
          (stackItem, stack) =>
            stackItem match
            case itemDescription(item) if item.nonEmpty =>
                result.getOrElseUpdate(stack, new mutable.Stack()).append(item.head)
            case other => //println(s"not parsing '$other'")
      }
    }
    result.toMap
  }


  private val instructionDescription = "move ([0-9]+) from ([0-9]+) to ([0-9]+)".r
  def parseInstructions(instructions: Iterator[String]): List[Instruction] =
    instructions.flatMap {
      case instructionDescription(count, from, to) => Some(Instruction(
        count = java.lang.Integer.parseInt(count),
        source = java.lang.Integer.parseInt(from),
        target = java.lang.Integer.parseInt(to),
      ))
      case other =>
        //println(s"INSTRUCTION PARSE FAILED FOR '$other'")
        None
    }.toList

  def readFile(name: String): (Map[Int, mutable.Stack[Char]], List[Instruction]) =
    val lines = Source.fromResource(name).getLines()
    val stacks = lines.takeWhile(s => !s.startsWith(" 1"))
    (parseStacks(stacks), parseInstructions(lines))


  def execute(file: (Map[Int, mutable.Stack[Char]], List[Instruction])): Map[Int, mutable.Stack[Char]] =
    val stacks = file._1
    val instructions = file._2
    for case Instruction(count, source, target) <- instructions do
      for _ <- 1 to count do
        val c = stacks(source-1).pop()
        //println(s"moving '$c' from $source to $target")
        stacks(target-1).push(c)
        //println("stacks after: " + stacks.toString())
    stacks

  def execute2(file: (Map[Int, mutable.Stack[Char]], List[Instruction])): Map[Int, mutable.Stack[Char]] =
    val stacks = file._1
    val instructions = file._2
    for case Instruction(count, source, target) <- instructions do
      var counter=0
      val c = stacks(source - 1).popWhile(_ =>
          counter += 1
          counter <= count
      )
      stacks(target - 1).pushAll(c.reverse)
    stacks

  def readStacks(stacks: Map[Int, mutable.Stack[Char]]): String =
    stacks
      .toList
      .sortBy(_._1)
      .map{
        (_, stack) => stack.pop
      }
      .mkString("")

  def printStacks(stacks: Map[Int, mutable.Stack[Char]]): Unit =
    for (i, s) <- stacks.toList.sortBy(_._1) do
      println(s"$i: " + s.toString())

  def example = readStacks(execute(readFile("day5/example.txt")))

  def star1 = readStacks(execute(readFile("day5/star1.txt")))

  def example2 = readStacks(execute2(readFile("day5/example.txt")))

  def star2 = readStacks(execute2(readFile("day5/star1.txt")))