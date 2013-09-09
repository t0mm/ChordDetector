package ru.t0mmy.chorddetector

import java.io.{FileReader, BufferedReader}

class Statistics(val inputLength : Int, val numRanks : Int, fileName : String) {
  import Resources._
  import Statistics._

  private val transPrefix = "<-"

  val data = readData
  
  private def readData = {
    var result = List.empty[Entry]
    for (r <- new FileReader(fileName);
         br <- new BufferedReader(r)) {
      def readTransitionProbs = {
        val l = br.readLine
        if (l != null && (l startsWith transPrefix))
          l.substring(transPrefix.length).split("\\s+").map { s => 
                                                              val fields = s split ":"
                                                              (fields(0), fields(1).toDouble) 
                                                            } .toMap
        else
          Map.empty[String, Double]
      }

      var finished = false
      do {
        val l = br.readLine
        if (l != null && !(l matches "\\s*")) {
          val classInfo = l split "\\s+"
          val entry = Entry(className = classInfo(0), 
                            classProb = classInfo(1).toDouble,
                            startProb = classInfo(2).toDouble,
                            transitionProbs = readTransitionProbs,
                            inputProbs = Array.fill[Double](inputLength, numRanks)(0.0))
          var i = 0 
          while (i < inputLength) {
            val probs = (br.readLine split "\\s+") map { _.toDouble }
            for (rank <- 0 until numRanks)
              entry.inputProbs(i)(rank) = probs(rank)
            i += 1  
          }
          result = entry :: result
        }
        else
          finished = true
      } while (!finished)
    }
    result
  }
}

object Statistics {
  case class Entry(className : String,
                   classProb : Double,
                   startProb : Double,
                   transitionProbs : Map[String, Double],
                   inputProbs : Array[Array[Double]])
}
