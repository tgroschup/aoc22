import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ArrayBuilder}
import scala.io.Source

object Day8 {
    def mapVisibilityFromLeft(s: Array[Byte]): Array[Boolean] =
      def mapVis(pos: Int, top: Byte, out: ArrayBuilder[Boolean]): Array[Boolean] =
        if(pos == s.length) out.result()
        else
          val newTop = if s(pos) > top then
            out.addOne(true)
            s(pos)
          else
            out.addOne(false)
            top
          mapVis(pos+1, newTop, out)
      mapVis(0, -1, new ArrayBuilder.ofBoolean)
    end mapVisibilityFromLeft



    def mapViewingDistanceToRight(s: Array[Byte]): Array[Long] =
      val x = for((tree, pos) <- s.zipWithIndex.init) yield {
        var score = 1;
        while(pos + score < s.length-1 && tree > s(pos + score))
          score += 1
        score.toLong
      }
      x :+ 0


    def mapVisibilityBothSides(s: Array[Byte]): Array[Boolean] = {
      mapVisibilityFromLeft(s)
        .zip(mapVisibilityFromLeft(s.reverse).reverse)
        .foldLeft(new ArrayBuilder.ofBoolean){
          case (array, (s1, s2)) => array.addOne(s1 | s2)
        }
        .result()
    }

  def mapVDBothSides(s: Array[Byte]): Array[Long] = {
    mapViewingDistanceToRight(s)
      .zip(mapViewingDistanceToRight(s.reverse).reverse)
      .foldLeft(new ArrayBuilder.ofLong) {
        case (array, (s1, s2)) => array.addOne(s1 * s2)
      }
      .result()
  }

  def mapVisibilityAllSides(s: Array[Array[Byte]]): Array[Array[Boolean]] = {
    val visLeftRight = s.map(mapVisibilityBothSides)
    val visUpDown = s.transpose.map(mapVisibilityBothSides).transpose

    visLeftRight.zip(visUpDown).foldLeft(new ArrayBuffer[Array[Boolean]]) {
      case (both, (leftRight, upDown)) => both.addOne(
        leftRight.zip(upDown).foldLeft(new ArrayBuilder.ofBoolean) {
          case (array, (s1, s2)) => array.addOne(s1 | s2)
        }.result
      )
    }.toArray
  }

      def mapVDAllSides(s: Array[Array[Byte]]): Array[Array[Long]] = {
        val visLeftRight = s.map(mapVDBothSides)
        val visUpDown = s.transpose.map(mapVDBothSides).transpose

        val zipped = visLeftRight.zip(visUpDown)
        zipped.foldLeft(new ArrayBuffer[Array[Long]]) {
          case (both, (leftRight, upDown)) => both.addOne(
            leftRight.zip(upDown).foldLeft(new ArrayBuilder.ofLong) {
              case (array, (s1, s2)) => array.addOne(s1 * s2)
            }.result
          )
        }.toArray
      }

    def readFile(name: String): Array[Array[Byte]] = {
      Source.fromResource(name)
        .getLines()
        .map {
          line => line.foldLeft(new ArrayBuilder.ofByte)(
            (array, c) => array.addOne(java.lang.Byte.parseByte(c.toString))
          )
        }.map(_.result()).toArray
    }

    def count1(s: Array[Array[Boolean]]): Long =
      s.map(_.map(b => if b then 1 else 0).sum).sum

    def printBoolArrays(s: Array[Array[Boolean]]): String =
      s.map(a => a.map(b => if b then 1 else 0).mkString(" "))
        .mkString("\n")

    def example = {
      count1(mapVisibilityAllSides(readFile("day8/example.txt")))
    }

    def star1 = count1(mapVisibilityAllSides(readFile("day8/star1.txt")))

    def example2 =
      val file = readFile("day8/example.txt")
      mapVDAllSides(file).flatten.max
      
    def star2 = mapVDAllSides(readFile("day8/star1.txt")).flatten.max
}
