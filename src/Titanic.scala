import scala.io.Source
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.collection.mutable.Map
import scala.util.Random

case class Passenger(
    passengerId: Int,
    survived: Option[Boolean],
    pclass: Int,
    name: String,
    sex: String,
    age: Int,
    sibSp: Int,
    parch: Int
)

object Titanic extends App {
  val trainData = readTrainData

  val criteria : Array[Passenger => Boolean] = Array(
    { p => p.pclass == 1 },
    { p => p.pclass == 2 },
    { p => p.pclass == 3 },

    { p => p.sex == "male" },
    { p => p.sex == "female" },

    { p => p.age <= 2 },
    { p => p.age >= 3 && p.age <= 6 },
    { p => p.age >= 7 && p.age <= 15 },
    { p => p.age >= 16 && p.age <= 25 },
    { p => p.age >= 26 && p.age <= 40 },
    { p => p.age >= 41 && p.age <= 60 },
    { p => p.age >= 61 },

    { p => p.parch == 0 },
    { p => p.parch != 0 }
  )

  val cDescriptions = Array(
      "1st class",
      "2nd class",
      "3rd class",
      "male",
      "female",

      "age <= 2",
      "3 <= age <= 6",
      "7 <= age <= 15",
      "16 <= age <= 25",
      "26 <= age <= 40",
      "41 <= age <= 60",
      "age >= 61",

      "has NO parents/children",
      "has parents/children"
  )

  val dTreeRules = buildDecisionTreeRules(trainData)

  val testData = readTestData

  val updatedTestData = testData map { p =>
    val key = getTreeLeafKey(p)
    val (survived, not) = dTreeRules.getOrElse(key, {
      if (p.sex == "female" && p.pclass == 1)
        (1, 0)
      else
      if (p.sex == "male" && p.pclass == 2)
        (0, 1)
      else
        throw new RuntimeException(s"key $key could not be bound for $p")
    })
    p.copy(survived = Some(survived > not))
  }

  outResults(updatedTestData)

  def getDescriptionFromKey(k: String) = {
    val subkeys = k.split("-")
    subkeys map { k =>
      cDescriptions(k.toInt)
    } mkString (" AND ")
  }

  def calcPercentSurvived(pair: (Int, Int)) = {
    (pair._1.toFloat * 100 / (pair._1.toFloat + pair._2.toFloat)).toInt
  }

  def doGuessSurvived(percentSurvived: Int) = {
    val guess = Random.nextInt(100) + percentSurvived
    guess >= 100
  }

  def showSorted(dTreeRules: Map[String, (Int, Int)]) = {
    val sortedKeys = dTreeRules.keys.toList.sortBy { k =>
      val (survived, not) = dTreeRules(k)
      survived - not
    }

    sortedKeys map { k =>
      val d = getDescriptionFromKey(k)
      val percentSurvived = calcPercentSurvived(dTreeRules(k))
      s"$d: $percentSurvived%, ${dTreeRules(k)}"
    } foreach (println)

  }

  def readTrainData() = {
    val train = Source.fromInputStream(getClass().getResourceAsStream("train.csv")).getLines().toArray
    val passengers = train.tail flatMap { line =>
      Try {
        val fields0 = line.split("\"")
        val name = fields0(1)
        val fields = s"${fields0(0)}<name>${fields0(2)}".split(",")
        Passenger(fields(0) toInt,
          Some(fields(1).toInt == 1),
          fields(2) toInt,
          name,
          fields(4),
          fields(5).toFloat toInt,
          fields(6) toInt,
          fields(7) toInt)
      } match {
        case Success(passenger) => Some(passenger)
        case Failure(ex)        => None
      }
    }
    passengers
  }

  def readTestData() = {
    val test = Source.fromInputStream(getClass().getResourceAsStream("test.csv")).getLines().toArray
    val passengers = test.tail map { line =>
      val fields0 = line.replace("\"\"", "").split("\"+")
      val name = fields0(1)
      val fields = s"${fields0(0)}<name>${fields0(2)}".split(",")
      Passenger(fields(0) toInt,
        None,
        fields(1) toInt,
        name,
        fields(3),
        if (fields(4).isEmpty()) 30 else fields(4).toFloat toInt,
        fields(5) toInt,
        fields(6) toInt)
    }
    passengers
  }

  def getTreeLeafKey(p: Passenger) = {
    criteria.foldLeft("") { (key, c) =>
      if (c(p)) {
        val cIndex = criteria.indexOf(c).toString
        if (key.isEmpty()) {
          cIndex
        } else {
          key + "-" + cIndex
        }
      } else
        key
    }
  }

  def buildDecisionTreeRules(passengers: Seq[Passenger]) = {
    val dTreeRules = Map[String, (Int, Int)]()

    val keys = passengers map { p =>
      val key = getTreeLeafKey(p)

      val (survived, not) = dTreeRules.getOrElse(key, (0, 0))
      val updatedKeyStats = if (p.survived.getOrElse(false)) {
        (survived + 1, not)
      } else {
        (survived, not + 1)
      }
      dTreeRules += key -> updatedKeyStats
    }

    dTreeRules
  }

  def outResults(updatedTestData: Seq[Passenger]) = {
    println("PassengerId,Survived")
    updatedTestData foreach { p =>
      val survived = if (p.survived.getOrElse(false)) 1 else 0
      println(s"${p.passengerId},${survived}")
    }
  }

  def outResultsDebug(updatedTestData: Seq[Passenger]) = {
    showSorted(dTreeRules)

    updatedTestData foreach { p =>
      println(p)
    }
  }

}
