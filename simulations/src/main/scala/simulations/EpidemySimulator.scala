package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    val prevalenceRate: Double = 0.01
    val transRate: Double = 0.40
    val dieRate: Double = 0.25
    val incubationTime = 6
    val dieTime = 14
    val immuneTime = 16
    val healTime = 18
  }

  import SimConfig._

  val persons: List[Person] = (for(i <- 1 to 300) yield new Person(i)).toList // to complete: construct list of persons

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false
    
    
    val directions = Vector((1,0),(-1,0),(0,1),(0,-1))

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    
    
    def getinfected = {
      if (!infected && !dead) {
        infected = true
        afterDelay(incubationTime)(sick = true)
        afterDelay(dieTime) {
          if (random < dieRate) {
            dead = true
          }
        }
        afterDelay(immuneTime) {
          if (!dead) {
            immune = true
          }
        }
        afterDelay(healTime) {
          if (!dead) {
            immune = false
            sick = false
            infected = false
          }
        }
      }
    }

    
    
    def move : Unit = {
      
     
      
      if(!dead){   
        afterDelay(randomBelow(5)+1)(move)
      }
    }
    
  }
}
