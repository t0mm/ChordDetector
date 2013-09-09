object ChordStats {
  case class Entry(var count : Int, 
                   transCounts : scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map.empty) 

  val suffixes = Array("sus2", "sus4", "m5-", "5+", "m", "")
  val alterations = Array("#", "b", "")
  val flatReplacements = List("Ab" -> "G#", "Db" -> "C#", "Eb" -> "D#", "Gb" -> "F#")
  
  private def replaceFlats(chord : String) = {
    flatReplacements.foldLeft (chord) { (a, r) => a.replaceAll(r._1, r._2) }
  }
  
  private def removeTags(s : String) = 
    s.replaceAll("<[^<>]*>", " ").replaceAll("^.*>", " ").replaceAll("<.*$", " ")

  def main(args : Array[String]) : Unit = {
    var firstChord : Option[String] = None
    var prevChord : Option[String] = None
    val stats = scala.collection.mutable.Map.empty[String, Entry]
    val english = args contains "-b"
  
    var rawLine = readLine
    while (rawLine != null) {
      val line = removeTags(rawLine)
      val words = line split "\\s+"
      for (w <- words if w != "") {
        val firstChar = w.head
        if ("ABCDEFGH" contains firstChar) {
          var processed = false
          for (s <- suffixes) 
            for (alt <- alterations) 
              if (!processed) {
                val tail = alt + s
                val l = tail.length + 1
                if ((w startsWith (firstChar + tail)) && (w.length <= l || !w.charAt(l).isLetter)) {
                  val rawChord = w.substring(0, l)
                  val chord = replaceFlats(if (english)
                                            rawChord.replaceAll("B", "H").replaceAll("Hb", "B")
                                           else
                                            rawChord)
                  if (firstChord == None)
                    firstChord = Some(chord)

                  if (stats contains chord)
                    stats(chord).count += 1
                  else 
                    stats(chord) = Entry(count = 1)
                  
                  prevChord match {
                    case Some(pc) => if (stats(chord).transCounts contains pc)
                                       stats(chord).transCounts(pc) += 1
                                     else
                                       stats(chord).transCounts(pc) = 1
                    case None =>
                  }

                  prevChord = Some(chord)
                  processed = true
                }
              }
        }
      }
        
      rawLine = readLine
    }
   
    firstChord match {
      case Some(c) => {
        println(c)
        for ((chord, cst) <- stats) {
          println(s"$chord ${cst.count}")
          print("<-")
          println(cst.transCounts map { e => s"${e._1}:${e._2}" } mkString " ")
        }
      }
      case None =>
    }
  }
}
