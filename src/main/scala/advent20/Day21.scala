package advent20

import advent20.utils.InputFileReader

import scala.collection.mutable

object Day21 extends App {
  val lines = new InputFileReader("input21").getLines
  val foods = lines.map(Food(_))

  val ingredientsByAllergens = allergenMaps(foods)
  val possibilities = allergenFoodPossibilities(ingredientsByAllergens)
  val possibleIngredients = possibilities.values.reduce(_ union _)

  val allIngredients = foods.flatMap(_.ingredients).toSet
  val cantPossibly = allIngredients -- possibleIngredients
  val answer1 = cantPossibly.toList.map(countIngredients(_, foods)).sum
  println("Answer (part 1) " + answer1)
  
  def allergenMaps(foods: List[Food]): Map[Allergen, List[Set[Ingredient]]] = {
    val allergensList = for (food <- foods; allergen <- food.allergens) yield allergen -> food.ingredients
    val allergens: mutable.Map[Allergen, List[Set[Ingredient]]] = mutable.Map.empty
    allergensList.foreach(entry => allergens.updateWith(entry._1) {
      case Some(ingredientsSets) => Some(entry._2.toSet :: ingredientsSets)
      case None => Some(List(entry._2.toSet))
    })
    allergens.toMap
  }

  def allergenFoodPossibilities(allergenMap: Map[Allergen, List[Set[Ingredient]]]): Map[Allergen, Set[Ingredient]] =
    allergenMap map { case (k, v) => k -> v.reduce(_ intersect _) }

  def countIngredients(ingredient: Ingredient, foods: List[Food]): Int = {
    val allIngredients = foods.flatMap(_.ingredients)
    allIngredients.count(_ == ingredient)
  }
}

case class Food(ingredients: List[Ingredient], allergens: List[Allergen])

case object Food {
  private val allergenMatcher = "contains (.*)\\)".r

  def apply(food: String): Food = {
    val split = food.split('(')
    val ingredients = split(0).split(' ').map(Ingredient).toList
    val allergens: List[Allergen] =
      if (split.length > 1)
        split(1) match {
          case allergenMatcher(allergenList) => allergenList.split(',').map(_.trim).map(Allergen).toList
        }
      else List.empty
    Food(ingredients, allergens)
  }
}

case class Ingredient(name: String)

case class Allergen(name: String)
