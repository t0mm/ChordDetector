package ru.t0mmy.chorddetector

import java.io.{FileInputStream, PrintWriter}

class ChordDetector(inFileName : String,
                    outFileName : String,
                    windowExponent : Int,
                    sampleRate : Int,
                    maxWindows : Int,
                    showRelativeAmp : Boolean,
                    showDump : Boolean,
                    shift : Double,
                    virtualShift : Int,
                    step : Int,
                    meanCount : Int,
                    bassShift : Int,
                    octaves : Int,
                    learn : (Boolean, String),
                    inputStatsFileName : String,
                    noTransProb : Double,
                    commonBonus : Double,
                    viterbiChainLength : Int = 7) {
  import Resources._
  import math._

  private class MaxInfo(m : Map[String, Double]) {
    private val me = maxItem(m) { e => e._2 }
    private val second = maxItem(m filterNot { e => e == me }) { e => e._2 }

    def chord = me._1
    override def toString = "%-16s" format "%s:%.2f/%.2f".format(me._1, me._2, me._2 / second._2)
  }

  private class RingBuffer {
    val capacity = 6
    val items = Array.ofDim[String](capacity)
    var firstIndex = -1
    var nextIndex = 0

    private def next(i : Int) = (i + 1) % capacity

    def push(value : String) = {
      items(nextIndex) = value
      if (firstIndex < 0)
        firstIndex = nextIndex
      else if (firstIndex == nextIndex)
        firstIndex = next(firstIndex)
      nextIndex = next(nextIndex)
    }

    def count(value : String) = 
      if (firstIndex < 0) 
        0 
      else {
        var r = 0
        var i = firstIndex
        do {
          if (items(i) == value)
            r += 1
          i = next(i)
        } while (i != nextIndex)
        r
      }
  }

  private val latestChords = new RingBuffer
  
  private val notes = Array("A", "B", "H", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#")
  private val aFreq = 440.0 / pow(2, bassShift)
  private val noteFreqs = 
      (0 until octaves * notes.length) map { n => aFreq * pow(2, n / 12.0) * pow(2, shift / 12) } 
  private val epsilon = pow(2, 1 / 48.0)
  
  private val chordPatterns = List(("",     List(1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0)),
                                   ("m",    List(1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0)),
                                   ("5+",   List(1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0)),
                                   ("m5-",  List(1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0)),
                                   ("sus2", List(1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0)),
                                   ("sus4", List(1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0)))
    
  private val windowSize = powerOfTwo(windowExponent)
  
  private val (isLearning, learningChord) = learn
  private val numRanks = 3
  private val learningStats = if (isLearning) Array.ofDim[Int](notes.length, numRanks) else null

  private val statistics = 
    if (inputStatsFileName != null) 
      new Statistics(notes.length, numRanks, inputStatsFileName) 
    else
      null

  private val virtualKeyShifter = new KeyShifter(notes, virtualShift)

  private val markovChainPostproc = 
    if (statistics != null)
      new MarkovChainPostprocessor(statistics, noTransProb, commonBonus, virtualKeyShifter)
    else
      null

  private val viterbiPostproc =
    if (statistics != null)
      new ViterbiPostprocessor(statistics, 0.2 /*noTransProb*/, 0.01 /*commonBonus*/, virtualKeyShifter)
    else
      null

  private val bayesianClassifier = 
    if (statistics != null)
      new BayesianClassifier(statistics, 
                             new MarkovChainPostprocessor(statistics, noTransProb, 
                                                          commonBonus, virtualKeyShifter))
    else
      null
  
  private def powerOfTwo(n : Int) : Int = n match {
    case 0 => 1
    case _ => 2 * powerOfTwo(n - 1)
  }

  private def maxItem[T](col : Iterable[T])(extract : T => Double) = 
  col.foldLeft(col.head) { (a, e) => if (extract(e) > extract(a)) e else a }
  
  private def idxToFreq(n : Int) = n * sampleRate / windowSize 

  private def freqToIdx(freq : Double) = round(freq * windowSize / sampleRate).toInt
  
  private def avgAmplitudeForFreq(amps : Array[Double], freq : Double) = {
    val (loFreq, hiFreq) = (freq / epsilon, freq * epsilon)
    val (loIdx, hiIdx) = (freqToIdx(loFreq), freqToIdx(hiFreq))
    
    ((loIdx to hiIdx) map { i => amps(i) } reduce { _ + _ }) / (hiIdx - loIdx + 1)
  }
  
  private def maxAmpInRange(amps : Array[Double], loIdx : Int, hiIdx : Int) = {
    var m = 0.0
    for (i <- loIdx to hiIdx)
      if (amps(i) > m)
        m = amps(i)
    m     
  }

  private def maxAmplitudeForFreq(amps : Array[Double], freq : Double) = {
    val (loFreq, hiFreq) = (freq / epsilon, freq * epsilon)
    val (loIdx, hiIdx) = (freqToIdx(loFreq), freqToIdx(hiFreq))
    maxAmpInRange(amps, loIdx, hiIdx)
  }

  private def hann(n : Int) = 0.5 * (1 - cos(2 * Pi * n / (windowSize - 1)))
  
  private def applyWindowFunction(data : Array[Double], func : Int => Double) = {
    val result = Array.fill[Double](data.length)(0)
    for (i <- 0 until data.length) 
      result(i) = data(i) * func(i)
    result
  }
  
  private def analyze(amps : Array[Double]) = {
    val noteAmps = ((0 until noteFreqs.length) map { i => maxAmplitudeForFreq(amps, noteFreqs(i)) }).toArray
    ((0 until 12) map { t => (0 until octaves).map { o => noteAmps(o * 12 + t) } 
                                              .reduce { _ + _} / octaves }).toArray
  }

  private def top3Notes(chromagram : Array[Double]) = {
    var top = List.empty[(Int, Double)]

    def idxOfMax = {
      var r = 0
      while (top.exists { n => n._1 == r })
        r += 1
        
      for (i <- 1 until chromagram.length)
        if (chromagram(i) > chromagram(r) && !top.exists { n => n._1 == i }) 
          r = i
      r
    }
        
    while (top.length < 3) {
      val m = idxOfMax
      top = (m, chromagram(m)) :: top
    }
    
    val maxAmp = top.last._2
    top.reverse map { n => (n._1, (n._2 * 100 / maxAmp).toInt, n._2) }
  }
  
  def tryGetChordName(chordNotes : List[Int]) = {
    def chordName(n1 : Int, n2 : Int, n3 : Int) = {
      def halftones(n : Int) = if (n > 0) n % 12 else n % 12 + 12
      val suffix = (halftones(n2 - n1), halftones(n3 - n2)) match {
        case (4, 3) => ""
        case (3, 4) => "m"
        case (4, 4) => "5+"
        case (3, 3) => "0"
        case (2, 5) => "sus2"
        case (5, 2) => "sus4"
        case _ => null
      }
      if (suffix != null)
        notes(n1) + suffix
      else
        null
    }
    val a :: b :: c :: Nil = chordNotes
    var r = chordName(a, b, c)
    if (r == null)
      r = chordName(b, a, c)
    if (r == null)
      r = chordName(a, c, b)
    if (r == null)
      r = chordName(c, a, b)
    if (r == null)
      r = chordName(b, c, a)
    if (r == null)
      r = chordName(c, b, a)
    if (r != null) r else "?"
  }
  
  private def patternMatchedLikelihoods(chromagram : Array[Double]) = {
    def rotatePattern(pattern : List[Int]) = pattern.last :: pattern.dropRight(1)
    def dotProduct(pattern : List[Int]) = (chromagram zip pattern) map { e => e._1 * e._2 } reduce { _ + _ } 
   
    var result = Map.empty[String, Double]
    var maxDotProd = 0.0

    for (p <- chordPatterns) {
      var curPat = p._2
      for (n <- 0 until 12) {
        val dp = dotProduct(curPat)
        result += (notes(n) + p._1) -> dp
        if (dp > maxDotProd)
          maxDotProd = dp
        curPat = rotatePattern(curPat)
      }
    }
    
    result map { e => e._1 -> e._2 / maxDotProd }
  }

  private def markovPostprocess(likelihoods : Map[String, Double]) = 
    markovChainPostproc.step(likelihoods)
  
  private def dump(amps : Array[Double], out : PrintWriter) = {
    val noteLowIndexes = noteFreqs map { f => freqToIdx(f / epsilon) } 
    val noteHiIndexes = noteFreqs map { f => freqToIdx(f * epsilon) }
    val (lowBound, hiBound) = (freqToIdx(noteFreqs(0)), freqToIdx(noteFreqs(noteFreqs.length - 1)))
    
    def getNote(idx : Int) = 
      (0 until noteFreqs.length).filter 
          { i => noteLowIndexes(i) <= idx && idx <= noteHiIndexes(i) } .toList match {
        case head :: _ => notes(head % 12)
        case Nil => ""
      } 

    val maxAmp = maxAmpInRange(amps, lowBound, hiBound)
    
    def chartColumn(idx : Int) = {
      (0 to (amps(idx) * 20 / maxAmp).toInt).map { i => '*' } .mkString
    }
      
    out.println("----------------------------------------------------------------")
    for (i <- lowBound to hiBound) {
      val freq = idxToFreq(i)
      val note = getNote(i)
      val amp = "%10d" format amps(i).toInt
      val col = chartColumn(i)
      out.println(s"$freq\t$note\t$amp\t$col")
    }
    out.println("----------------------------------------------------------------")
  }
  
  private def rank(amp : Double, maxAmp : Double) = min((amp * numRanks / maxAmp).toInt, numRanks - 1)
  
  private def classify(chromagram : Array[Double], maxAmp : Double) = {
    val ranks = chromagram map { a => rank(a, maxAmp) }
    bayesianClassifier.classes(ranks)
  }
  
  private def processWindow(data : Array[Double]) = {
    val result = FFT.transform(applyWindowFunction(data, hann))
    result  
  }
  
  private def windowNumberToTime(n : Int) = {
    val secs = n * (if (step == 0 || step > windowSize) windowSize else step) / sampleRate
    "%02d:%02d".format(secs / 60, secs % 60)
  }
  
  private def readSamples(stream : FileInputStream, count : Int) = {
    val data = Array.fill[Byte](count * 2)(0)
    val r = stream.read(data)
    (data, r == data.length)
  }
  
  private def convertSamples(data : Array[Byte], doubleData : Array[Double], doubleOffset : Int) = {
    for (i <- 0 until data.length / 2)
      doubleData(doubleOffset + i) = (data(2 * i) & 0xFF) | (data(2 * i + 1) << 8)
  }
  
  private def fillWindow(stream : FileInputStream, doubleData : Array[Double]) = {
    val (data, readEnough) = readSamples(stream, windowSize)
    if (readEnough) {
      convertSamples(data, doubleData, 0)
      true
    }
    else
      false
  }
  
  private def shiftWindow(stream : FileInputStream, doubleData : Array[Double]) = {
    if (step == 0 || step > windowSize)
      fillWindow(stream, doubleData)
    else {
      val (data, readEnough) = readSamples(stream, step)
      if (readEnough) {
        Array.copy(doubleData, step, doubleData, 0, doubleData.length - step)
        convertSamples(data, doubleData, doubleData.length - step)
        true
      }
      else
        false
    }
  }

  private def best(chord1 : String, chord2 : String) = {
    latestChords push chord1
    latestChords push chord2
    if ((latestChords count chord2) > (latestChords count chord1)) chord2 else chord1  
  }

  private def checkNewLoudNotes(top3 : List[(Int, Int, Double)], prevChroma : Array[Double]) =
    if (prevChroma == null)
      true
    else {
      var r = false
      for (e <- top3) {
        val prevAmp = prevChroma(e._1)
        if (prevAmp > 0 && e._3 / prevAmp > 2)
          r = true
      }
      r
    }
    
  def proceed = {
    for (in <- new FileInputStream(inFileName);
         out <- new PrintWriter(outFileName)) {
      val data = Array.fill[Double](windowSize)(0)
      var r = fillWindow(in, data)
      var cnt = 0
      var chromas = List.empty[Array[Double]]
      var viterbiCount = 0
      var prevChroma : Array[Double] = null
      while (r && cnt < maxWindows) {
        val amps = processWindow(data)
        val chromagram = analyze(amps)
     
        if (isLearning) {
          val top = top3Notes(chromagram)
          val maxAmp = top.head._3
          for (i <- 0 until chromagram.length) {
            val r = rank(chromagram(i), maxAmp)
            learningStats(i)(r) += 1
          }
        }
        else {
          def maxInfo(m : Map[String, Double]) = new MaxInfo(m)

          def calcMeanChroma = {
            val summary = chromas reduce { (a, c) => a zip c map { e => e._1 + e._2 } }
            summary map { e => e / chromas.length }
          }

          if (meanCount > 1)
            chromas = chromagram :: chromas

          if (meanCount == 1 || chromas.length == meanCount) {
            val meanChroma = if (meanCount == 1) chromagram else calcMeanChroma
            val top = top3Notes(meanChroma) 
            val maxAmp = top.head._3
            val time = windowNumberToTime(cnt)
            val topNote = notes(top.head._1)
            val maxAmpStr = "%10d" format maxAmp.toInt   
            val noteNames = 
                top map { n => notes(n._1) + (if (showRelativeAmp) s":${n._2}" else "") } mkString(", ")
            val chordName = tryGetChordName(top.map(_._1))
            val newLoudNotes = checkNewLoudNotes(top, prevChroma)
            val newLoudFlag = if (newLoudNotes) '*' else ' '

            val patMatLikes = patternMatchedLikelihoods(meanChroma)
            val patMatNearestChordInfo = maxInfo(patMatLikes)
            //println(patMatLikes.toList.sortBy { e => -e._2 })
            val postprocPatMatNearestChordInfo = 
              if (markovChainPostproc != null) {
                val ppLikes = markovChainPostproc.step(patMatLikes)
                //println(ppLikes.toList.sortBy { e => -e._2 })
                maxInfo(ppLikes)
              }
              else
                null

            if (viterbiPostproc != null && newLoudNotes) {
              viterbiPostproc.step(patMatLikes)
              viterbiCount += 1
              if (viterbiCount == viterbiChainLength) {
                out.println(viterbiPostproc.guessChain mkString " ")
                viterbiCount = 0
              }
            }
            
            val bayesianChordInfo = if (bayesianClassifier != null) {
                                      val cs = classify(meanChroma, maxAmp)
                                      maxInfo(cs)
                                    }
                                    else
                                      null

            val postprocPatMatNearestChordInfoStr = if (postprocPatMatNearestChordInfo != null)
                                                      postprocPatMatNearestChordInfo.toString
                                                    else
                                                      ""
            val bayesianChordInfoStr = if (bayesianChordInfo != null)
                                         bayesianChordInfo.toString
                                       else
                                         ""

            val bestChordOfTwo = 
              "%-8s".format(if (postprocPatMatNearestChordInfo != null && bayesianChordInfo != null) 
                              best(postprocPatMatNearestChordInfo.chord, bayesianChordInfo.chord)
                            else 
                              "")

            val topNotesAndChord = "%-35s" format s"$topNote ($noteNames) -> $chordName"                        
            val left = s"$time $maxAmpStr $newLoudFlag $topNotesAndChord " +
                       s"$patMatNearestChordInfo $postprocPatMatNearestChordInfoStr $bayesianChordInfoStr" +
                       s"$bestChordOfTwo"

            val right = (0 until 12) map 
                            { i => "%s %3d".format(notes(i), (100 * meanChroma(i) / maxAmp).toInt) } mkString "|"
            out.println("%-100s %s".format(left, right))

            prevChroma = meanChroma

            if (meanCount > 1)
              chromas = Nil
          }
          if (showDump)
            dump(amps, out)
        }
        r = shiftWindow(in, data)
        cnt += 1
      }
      
      if (isLearning) {
        out.println(learningChord)
        for (s <- learningStats) 
          out.println(s map { n => max(n, 1).toString } reduce { _ + " " + _ })
      }
    }
  }
}
