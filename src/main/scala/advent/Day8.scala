package advent

import scala.io.Source
import scala.util.Using

object Day8 extends App {

  val imageLayers = Using(Source.fromFile("src/main/data/day8-image.txt")) { file => file.getLines().toSeq.head }
    .get
    .toSeq
    .map(c => Integer.parseInt(c.toString))

  val width = 25
  val height = 6
  val size = width * height


  val (layer, solution) = {

    val (layer, countedPixels) = imageLayers

      // slice images into layers
      .grouped(size)
      .zipWithIndex

      // count pixel of each type in each layer
      .map { case (layer: IndexedSeq[Int], index: Int) => (index, layer.groupBy(c => c).view.mapValues(_.size).toMap) }
      .reduce((a, b) => if (a._2(0) < b._2(0)) a else b)

    (layer, countedPixels(1) * countedPixels(2))
  }

  println(s"Advent of code 2019 - Day 8 / part 1: $layer,  solution: $solution") // 2562


  val decodedImage = imageLayers
    // slice images into layers
    .grouped(size)

    // stack pixels together
    .toList
    .transpose

    // keep the first non transparent pixel
    .map(_.dropWhile(_ == 2).head)


  println(s"Advent of code 2019 - Day 8 / part 2: decoded image:") // ZFLBY
  // poor man's print, using space for white and "|" for black
  decodedImage
    .grouped(width)
    .foreach(s => println(s.mkString("").replace("0", " ").replace("1", "|")))
}
