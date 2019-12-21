package advent

import advent.Day14.MapMonoid

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day14Part1 extends App {

  lazy val rawRecipe = Using(Source.fromFile("src/main/data/day14-recipe.txt")) { file => file.getLines().toList }.get
  lazy val cookbook = Day14.cookBook(rawRecipe)

  def requiredOresForOneFuel(book: Map[String, Day14.Recipe]): Long = {
    Day14.totalOres(0, Map("FUEL" -> 1), Map.empty)(book)._1
  }

  val ores = requiredOresForOneFuel(cookbook)

  println(s"Advent of code 2019 - Day 14 / part 1: ores for fuel: ${ores}") // 469536

}

object Day14Part2 extends App {

  lazy val rawRecipe = Using(Source.fromFile("src/main/data/day14-recipe.txt")) { file => file.getLines().toList }.get
  lazy val cookbook = Day14.cookBook(rawRecipe)

  val fuel = fuelFor1TOres(cookbook)
  println(s"Advent of code 2019 - Day 14 / part 2: fuel for 1T ores: ${fuel}") // 3343477

  // solution to one iteration
  case class Solution(fuel: Long, oresCost: Long, leftOvers: Map[String, Long], book: Map[String, Day14.Recipe]) {
    def +(other: Solution): Solution = {
      val oresApprox = oresCost + other.oresCost
      val leftOversApprox = MapMonoid.add(leftOvers, other.leftOvers)
      val (extrapolatedOres, extrapolatedLetfOvers) = backTrack(oresApprox, leftOversApprox)(book)
      Solution(fuel + other.fuel, extrapolatedOres, extrapolatedLetfOvers, book)
    }
  }


  // main algo for part to: highest amount of fuel we can produce with 1T ores
  def fuelFor1TOres(book: Map[String, Day14.Recipe]): Long = {

    val targetCost = 1000000000000L

    @tailrec
    def producedFuel(solutions: Set[Solution]): Solution = {

      val highestSolution = solutions.maxBy(_.oresCost)

      val availableMoney = targetCost - highestSolution.oresCost
      solutions.filter(_.oresCost <= availableMoney).maxByOption(_.oresCost) match {
        case Some(mostExpensiveKnow) => producedFuel(solutions + (mostExpensiveKnow + highestSolution))
        case None => {

          // for the last steps, we can iterate manually
          val (lastCost, lastLeftOvers) = Day14.totalOres(highestSolution.oresCost, Map("FUEL" -> 1), highestSolution.leftOvers)(book)
          val oneMoreSolution = Solution(highestSolution.fuel +1, lastCost, lastLeftOvers, book)
          if (lastCost < targetCost) producedFuel(solutions + oneMoreSolution)
          else highestSolution
        }
      }
    }

    val (oreCost1, leftOvers) = Day14.totalOres(0, Map("FUEL" -> 1), Map.empty)(book)
    producedFuel(Set(Solution(1, oreCost1, leftOvers, book))).fuel

  }


  // give that ore cost and left-overs to produce that amount of fuel, extrapolate the corresponding cost for the specified factor
  def extrapolatedCost(fuel: Long, oresCost: Long, leftOvers: Map[String, Long], factor: Int)
                      (implicit book: Map[String, Day14.Recipe]): (Long, Long, Map[String, Long]) = {

    val oresApprox = oresCost * factor
    val leftOversApprox = leftOvers.map { case (chem, count) => (chem, count * factor) }

    val (extrapolatedOres, extrapolatedLetfOvers) = backTrack(oresApprox, leftOversApprox)

    (fuel * factor, extrapolatedOres, extrapolatedLetfOvers)
  }


  @tailrec
  def backTrack(oresApprox: Long, leftOverApprox: Map[String, Long])(implicit book: Map[String, Day14.Recipe]): (Long, Map[String, Long]) = {

    leftOverApprox
      .toList.sortBy(_._1) // sorting just to help debugging
      .find { case (chem, qty) => qty >= book(chem).producedQty } match {

      // this leftOver is > than what we produce in one step => let's backtrack the application of that rule
      case Some((chemical, qty)) =>

        val recipe = book(chemical)

        // in case this recipe required ores, we don't need them
        val correctedOresCost = oresApprox - recipe.ingredients.find(_.name == "ORE").map(_.qty).getOrElse(0L)

        // replace that quantity of chemical by its components
        val addedIngredients = recipe.ingredients
          .filter(_.name != "ORE")
          .map(chem => chem.name -> chem.qty)
          .toMap

        val correctedLeftOver = (
          MapMonoid.add(leftOverApprox, addedIngredients)
            + (chemical -> (qty - recipe.producedQty))
          ).filter(_._2 > 0)

        backTrack(correctedOresCost, correctedLeftOver)

      // no leftover is over threshold, all good
      case None => (oresApprox, leftOverApprox)
    }
  }


}

object Day14 {

  case class Chemical(name: String, qty: Long)

  object Chemical {
    def apply(str: String): Chemical =
      Chemical(
        str.dropWhile(_.isDigit),
        str.takeWhile(_.isDigit).toLong
      )
  }

  case class Recipe(producedQty: Long, ingredients: Set[Chemical])

  // parses one single recipe into a map of one element
  def recipe(oneLineDescription: String): Map[String, Recipe] = {

    val ingredients :: resulting :: Nil = oneLineDescription
      .replace(" ", "")
      .split("=>")
      .toList

    Map(
      Chemical(resulting).name ->
        Recipe(
          Chemical(resulting).qty,
          ingredients.split(",").toSet.map(Chemical.apply)
        )
    )
  }

  // parses a multi-line recipe cookbook
  def cookBook(descriptions: Seq[String]): Map[String, Recipe] = descriptions.map(recipe).reduce(_ ++ _)


  // computes the amount of ores to create all those ingredients
  // returns the amount of ores, and left over ingredients
  @tailrec
  def totalOres(oresSoFar: Long, ingredients: Map[String, Long], leftOvers: Map[String, Long])
               (implicit book: Map[String, Recipe]): (Long, Map[String, Long]) =

  // look at the "first" ingredient of the map (sorting to make everything deterministic and easier to debug)
    ingredients.toList.sortBy(_._1) match {

      case first :: remaining =>
        val ingredient = Chemical(first._1, first._2)
        val remainingIngredients = remaining.toMap

        if (ingredient.name == "ORE") totalOres(oresSoFar + ingredient.qty, remainingIngredients, leftOvers)

        else {
          val (maybeStillRequired, newLeftOvers) = fromLeftOvers(ingredient, leftOvers)

          maybeStillRequired match {
            case Some(stillRequired) =>

              val (moreIngredients, moreLeftOvers) = cook(stillRequired, book(stillRequired.name))

              totalOres(
                oresSoFar,
                MapMonoid.add(remainingIngredients, moreIngredients),
                MapMonoid.add(newLeftOvers, moreLeftOvers))

            case None => totalOres(oresSoFar, remainingIngredients, newLeftOvers)
          }
        }

      case _ => (oresSoFar, leftOvers)
    }


  // looks up ingredients in the letfovers.
  // return the ingredients still needed and the updated leftovers
  def fromLeftOvers(required: Chemical, leftOvers: Map[String, Long]): (Option[Chemical], Map[String, Long]) =

    leftOvers.get(required.name) match {

      case Some(available) if available >= required.qty =>
        (None, leftOvers + (required.name -> (available - required.qty)))

      case Some(notEnough) => (
        Some(required.copy(qty = required.qty - notEnough)),
        leftOvers.removed(required.name))

      case None => (Some(required), leftOvers)
    }

  // tries to cook the demanded amount of that chemical
  // returns the required ingredients, plus the optional leftovers (maybe with zero quantity)
  def cook(demanded: Chemical, recipe: Recipe): (Map[String, Long], Map[String, Long]) = {

    val factor = (demanded.qty.doubleValue / recipe.producedQty).ceil.longValue
    val ingredients = recipe.ingredients
      .map(chemical => (chemical.name -> (chemical.qty * factor)))
      .toMap

    (ingredients, Map(demanded.name -> ((recipe.producedQty * factor) - demanded.qty)))

  }

  object MapMonoid {
    def empty = Map.empty[String, Long]

    def add(l1: Map[String, Long], l2: Map[String, Long]): Map[String, Long] =
      (l1.keySet ++ l2.keySet)
        .map {
          name =>
            name -> ((l1.get(name), l2.get(name)) match {
              case (Some(q1), Some(q2)) => q1 + q2
              case (Some(q1), None) => q1
              case (None, Some(q2)) => q2
            })
        }
        .toMap
        .filter(_._2 != 0)
  }

}
