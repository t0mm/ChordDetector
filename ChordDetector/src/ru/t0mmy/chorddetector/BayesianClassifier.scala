package ru.t0mmy.chorddetector

class BayesianClassifier(stats : Statistics, markovChainPostproc : MarkovChainPostprocessor = null) {
  def classes(input : Array[Int]) = {
    def likelihood(classEntry : Statistics.Entry) = 
      (0 until stats.inputLength).map { i => classEntry.inputProbs(i)(input(i)) } 
                                 .reduce { _ * _ * stats.numRanks }  
    
    if (markovChainPostproc != null) {
      val likelihoods = stats.data.map { e => e.className -> likelihood(e) } .toMap  
      markovChainPostproc.step(likelihoods)
    }
    else
      stats.data.map { e => e.className -> e.classProb * likelihood(e) } .toMap
  }
  
  def topmostClass(input : Array[Int]) = {
    val cl = classes(input)
    var top = cl.head
    for ((k, v) <- cl)
      if (v > top._2)
        top = (k, v)
     top   
  }
}
