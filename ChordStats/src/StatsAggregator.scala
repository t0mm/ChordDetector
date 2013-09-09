import java.io.{File, FilenameFilter, FileReader, BufferedReader}

object StatsAggregator extends App {
  private class Filter(ext : String) extends FilenameFilter {
    def accept(dir : File, name : String) = name endsWith ext
  }
 
  private val transPrefix = "<-"
  private val numRanks = 3
  
  // maps chord symbol to (chord probability, 
  //                       probability that this chord is the first chord in the song,
  //                       transition probabilities,
  //                       distribution of rank probabilities for each tone in chromatic gamma)
  private case class Entry(var chordProb : Double = 0.0, 
                           var chordIsFirstProb : Double = 0.0,
                           transitionProbs : scala.collection.mutable.Map[String, Double] = 
                             scala.collection.mutable.Map.empty,
                           noteProbs : Array[Array[Double]] = Array.fill[Double](12, numRanks)(0.0)) 
  private var result = Map.empty[String, Entry]
  
  private def resultEntry(chord : String) = 
    result.get(chord).orElse {
      val e = Entry()
      result += chord -> e
      Some(e)
    }.get

  private def addNoteStats(e : Entry, noteIdx : Int, stats : Array[Double]) = 
    for (i <- 0 until stats.length)
      e.noteProbs(noteIdx)(i) += stats(i)
      
  private def readNonEmptyLine(br : BufferedReader) = {
    var l : String = null  
    do 
      l = br.readLine
    while (l != null && (l matches "\\s*")) 
    l
  }
      
  private def readChromaDistribution(fileName : String) = {
    val r = new FileReader(fileName)  
    val br = new BufferedReader(r)
    try {
      val chord = readNonEmptyLine(br)
      val entry = resultEntry(chord)
      var note = 0 
      var l = readNonEmptyLine(br) 
      while (l != null) {
        val counts = (l split "\\s+") map { _.toInt }
        for (rank <- 0 until numRanks)
          entry.noteProbs(note)(rank) += counts(rank)
        note += 1  
        l = readNonEmptyLine(br)
      }
    }
    finally {
      br.close
      r.close
    }
  }      
  
  private def readChordsDistribution(fileName : String) = {
    val r = new FileReader(fileName)  
    val br = new BufferedReader(r)
    try {
      var l = readNonEmptyLine(br)
      resultEntry(l).chordIsFirstProb += 1
      l = readNonEmptyLine(br)
      while (l != null) {
        val fields = l split "\\s+"
        val chord = fields(0)
        resultEntry(chord).chordProb += fields(1).toInt
        l = readNonEmptyLine(br)
        if (l != null && (l startsWith transPrefix)) {
          if (l.trim != transPrefix) {
            val transStats = l.substring(transPrefix.length).split("\\s+")
                                                            .map { f => f split ":" }
            for (t <- transStats if t.length == 2) {
              val fromChord = t(0)
              val count = t(1).toInt
              if (resultEntry(chord).transitionProbs contains fromChord)
                resultEntry(chord).transitionProbs(fromChord) += count
              else
                resultEntry(chord).transitionProbs(fromChord) = count
            }
          }
          l = readNonEmptyLine(br)
        } 
      }
    }
    finally {
      br.close
      r.close
    }
  } 
  
  private def normalizeData {
    def sum[T](col : Iterable[T])(extract : T => Double) =
      col.foldLeft(0.0) { (a, e) => a + extract(e) }

    val totalChordProb = sum(result.values) { e => e.chordProb }
    val totalChordIsFirstProb = sum(result.values) { e => e.chordIsFirstProb }
    val totalTransProbs = 
      result map { e => e._1 -> sum(result.values) { v => v.transitionProbs.get(e._1).orElse(Some(0.0)).get } }

    for (e <- result.values) {
      if (totalChordProb != 0)
        e.chordProb /= totalChordProb
      if (totalChordIsFirstProb != 0)
        e.chordIsFirstProb /= totalChordIsFirstProb

      for (c <- e.transitionProbs.keys) {
        val ttp = totalTransProbs(c)
        if (ttp != 0)
          e.transitionProbs(c) /= ttp
      }

      for (n <- 0 until 12) {
        val totalRankProb = sum(e.noteProbs(n)) { x => x }
        if (totalRankProb != 0)
          for (r <- 0 until numRanks)
            e.noteProbs(n)(r) /= totalRankProb
      }
    }
  }
  
  private def processSourceFiles = {
    val curDir = new File(".")
    
    for (f <- curDir.listFiles(new Filter(".chroma")))
      readChromaDistribution(f.getPath)
      
    for (f <- curDir.listFiles(new Filter(".chords")))
      readChordsDistribution(f.getPath)
    
    normalizeData
  }
  
  private def writeOutput = {
    for ((chord, probs) <- result) {
      println(s"$chord ${probs.chordProb} ${probs.chordIsFirstProb}")
      print(transPrefix)
      println(probs.transitionProbs map { e => s"${e._1}:${e._2}" } mkString " ")
      for (n <- 0 until 12)
        println(probs.noteProbs(n) mkString " ")
    }
  }
  
  processSourceFiles
  writeOutput
}
