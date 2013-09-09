package ru.t0mmy.chorddetector

class MarkovChainPostprocessor(stats : Statistics, 
                               noTransProb : Double, 
                               commonBonus : Double,
                               virtualKeyShifter : KeyShifter) {
  private val statMap = stats.data.map { e => e.className -> e } .toMap

  private var prevResult : Map[String, Double] = null

  private def shift(chord : String) = virtualKeyShifter.shift(chord) 

  private def sum[T](col : Iterable[T])(extract : T => Double) =
    col.foldLeft(0.0) { (a, e) => a + extract(e) }

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

  private def totalTransProb(toChord : String) = 
    sum(prevResult) { e => e._2 * transProb(e._1, toChord) }

  private def normalize(probs : Map[String, Double]) = {
    val s = sum(probs) { e => e._2 }
    probs map { e => e._1 -> e._2 / s }
  }

  def step(inputProbs : Map[String, Double]) = {
    val r = if (prevResult == null)
              inputProbs map { e => e._1 -> e._2 * startProb(e._1) }
            else {
              inputProbs map { e => e._1 -> e._2 * totalTransProb(e._1) }
            }
    val nr = normalize(r)
    prevResult = nr
    nr
  }
}
