package simulations

import math.random
import scala.util.Random


/* Code Lost  
 use https://github.com/robertberry/Principles-of-Reactive-Programming-Homework instead 
*/
object Utils {
  def mod(n: Int, m: Int) = n % m match {
    case i if i < 0 => i + m
    case i => i
  }

  /** Creates a neighbours function for a grid of given rows and cols */
  def makeNeighbours(rows: Int, cols: Int) =
    (row: Int, col: Int) => for {
      (r, c) <- List(
        (row + 1, col),
        (row - 1, col),
        (row, col + 1),
        (row, col - 1)
      )
    } yield (mod(r, rows), mod(c, cols))
}

class EpidemySimulator extends Simulator {
  val NumberOfPersons = 300
  val InfectionPrevalence = 0.01
  val NumberOfInitiallyInfected = (NumberOfPersons * InfectionPrevalence).toInt
  val ChanceOfDying = 0.25
  val TransmissabilityRate = 0.4
  val AirTrafficChance = 0.01
  val VaccinationRate = 0.05
  val NumberVaccinated = (NumberOfPersons * VaccinationRate).toInt

  def randomBelow(i: Int) = (random * i).toInt
  def randInt(low: Int, high: Int) = randomBelow(high - low) + low
  /** Returns true with n chance (n being a number between 0.0 and 1.0) */
  def chance(n: Double) = Random.nextDouble() < n
  def choice[A](xs: List[A]) = xs(randomBelow(xs.length))

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    val airTraffic = false
    val reduceMobility = false
    val chosenFew = false
  }

  import SimConfig._

  val persons: List[Person] =
    ((1 to NumberOfPersons) map { id =>
      new Person(id, vaccinated = chosenFew && id >= NumberOfPersons - NumberVaccinated)
    }).toList

  persons.take(NumberOfInitiallyInfected).foreach(_.infect)

  val neighbourCoords = Utils.makeNeighbours(SimConfig.roomRows, SimConfig.roomColumns)

  def personsAtCoord(row: Int, col: Int) = persons.filter(person => person.row == row && person.col == col)

  def okToMoveTo(row: Int, col: Int) =
    !personsAtCoord(row, col).exists(_.visiblyInfected)

  def isActuallyOk(row: Int, col: Int) =
    !personsAtCoord(row, col).exists(_.infected)

  class Person (val id: Int, val vaccinated: Boolean = false) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def visiblyInfected = sick || dead

    def infect {
      if (!vaccinated) {
        infected = true
        afterDelay(6) {
          sick = true
        }
        afterDelay(14) {
          if (chance(ChanceOfDying)) {
            dead = true
          } else {
            afterDelay(2) {
              if (!dead) {
                /** This basically to pass the test suite - so far as I can reason about the program (and it's difficult
                  * with all the mutable state), I don't think this would ever happen in a normal run
                  */
                sick = false
                immune = true
              }
            }
            afterDelay(4) {
              if (!dead) {
                immune = false
                infected = false
              }
            }
          }
        }
      }
    }

    //
    // to complete with simulation logic
    //

    def moveWithinDelay {
      val baseDelay = randInt(1, 5)

      val delayInDays = if (reduceMobility) {
        if (visiblyInfected) {
          baseDelay * 4
        } else {
          baseDelay * 2
        }
      } else {
        baseDelay
      }

      afterDelay(delayInDays) {
        move
      }
    }

    private def moveAndInfect(newRow: Int, newCol: Int) {
      /** Calculate whether gets infected */
      if (!isActuallyOk(newRow, newCol) && !infected && chance(TransmissabilityRate)) {
        infect
      }

      row = newRow
      col = newCol
    }

    private def hopAPlain {
      val newRow = randomBelow(roomRows)
      val newCol = randomBelow(roomColumns)

      moveAndInfect(newRow, newCol)
    }

    private def moveToAdjacent {
      val newPosition = neighbourCoords(row, col) filter { case ((r, c)) => okToMoveTo(r, c) } match {
        case Nil => None
        case xs => Some(choice(xs))
      }

      newPosition foreach {
        case (newRow, newCol) => moveAndInfect(newRow, newCol)
      }
    }

    def move {
      if (!dead) {
        if (airTraffic && chance(AirTrafficChance)) {
          hopAPlain
        } else {
          moveToAdjacent
        }

        moveWithinDelay
      }
    }

    moveWithinDelay
  }
}