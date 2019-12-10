package advent

import scala.io.Source
import scala.util.Using

object Day8Part1 extends App {

  val image = Using(Source.fromFile("src/main/data/day8-image.txt")) { file => file.getLines().toSeq.head }
    .get
    .toSeq
    .map(c => Integer.parseInt(c.toString))

  val size = 25 * 6

  val (layer, countedPixels) = image

    // slice images
    .grouped(size)
    .zipWithIndex

    // count pixel of each type in each layer
    .map{ case(layer: IndexedSeq[Int], index: Int) => (index, layer.groupBy(c => c).view.mapValues(_.size).toMap)}
    .reduce((a,b) => if (a._2(0) < b._2(0)) a else b)

  val solution = countedPixels(1) * countedPixels(2)
  println(s"selected layer: $layer, couts: $countedPixels, solution: $solution")  // 2562

}
