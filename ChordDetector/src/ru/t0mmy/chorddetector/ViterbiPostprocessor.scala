package ru.t0mmy.chorddetector

class ViterbiPostprocessor(stats : Statistics, 
                           noTransProb : Double, 
                           commonBonus : Double,
                           virtualKeyShifter : KeyShifter) {
  private val statMap = stats.data.map { e => e.className -> e } .toMap

  private var prevProbs : Map[String, Double] = null
  private var mostLikelyTransitions = List.empty[Map[String, String]]

  private def shift(chord : String) = virtualKeyShifter.shift(chord) 

  private def startProb(chord : String) = {
    val shChord = shift(chord)
    val p = if (statMap contains shChord)
              statMap(shChord).startProb
            else
              0.0
    p + commonBonus
  }

  private def transProb(fromChord : String, toChord : String) = {
   val p = if (fromChord == toChord)
              noTransProb
            else {
              val shFromChord = shift(fromChord)
              val shToChord = shift(toChord)
              if (statMap contains shToChord)
                statMap(shToChord).transitionProbs.get(shFromChord).orElse(Some(0.0)).get
              else
                0.0
            }

    p + commonBonus
  }

  private def mapMax[K, V](m : Map[K, V])(extract : (K, V) => Double) = {
    var maxValue : Option[Double] = None
    var maxKey : Option[K] = None
    for ((k, v) <- m) {
      val d = extract(k, v)
      if (maxValue == None || d > maxValue.get) {
        maxValue = Some(d)
        maxKey = Some(k)
      }
    }

    (maxKey.get, maxValue.get)
  }

  private def maxTransProb(toChord : String, chromaProb : Double) = 
    mapMax(prevProbs) { (chord, prob) => prob * transProb(chord, toChord) * chromaProb }

  def step(inputProbs : Map[String, Double]) = {
    val r = if (prevProbs == null)
              inputProbs map { e => e._1 -> ("", e._2 * startProb(e._1)) }
            else {
              inputProbs map { e => e._1 -> maxTransProb(e._1, e._2) }
            }
    mostLikelyTransitions = (r map { e => e._1 -> e._2._1 }) :: mostLikelyTransitions       
    prevProbs = r map { e => e._1 -> e._2._2 }
  }

  def guessChain = {
    val (lastChord, lastProb) = mapMax(prevProbs) { (chord, prob) => prob }
    var chain = lastChord :: Nil
    for (t <- mostLikelyTransitions)
      chain = t(chain.head) :: chain
    /*
    println(prevProbs)
    println(mostLikelyTransitions)
    println(chain)
    println("---")
    */
    prevProbs = null
    mostLikelyTransitions = Nil
    chain
  }
}
