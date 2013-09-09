package ru.t0mmy.chorddetector

import java.io.FileInputStream

object Main {
  private var inFileName = "in.s16"
  private var outFileName = "out.txt"
  private var windowExponent = 13
  private var sampleRate = 44100
  private var maxWindows = 100
  private var showRelativeAmp = true
  private var showDump = false
  private var shift = 0.0
  private var step = 0
  private var meanCount = 1
  private var bassShift = 1
  private var octaves = 3
  private var learn = false -> ""
  private var inputStatsFileName : String = null
  private var noTransProb = 0.2
  private var commonBonus = 0.1
  private var virtualShift = 0

  val optionsWithVals : Map[String, String => Unit] = Map("-e" -> { s => windowExponent = s.toInt },
                                                          "-r" -> { s => sampleRate = s.toInt },
                                                          "-n" -> { s => maxWindows = s.toInt },
                                                          "-s" -> { s => shift = s.toDouble },
                                                          "-v" -> { s => virtualShift = s.toInt },
                                                          "-t" -> { s => step = s.toInt },
                                                          "-m" -> { s => meanCount = s.toInt },
                                                          "-b" -> { s => bassShift = s.toInt },
                                                          "-o" -> { s => octaves = s.toInt },
                                                          "-l" -> { s => learn = true -> s},
                                                          "-y" -> { s => inputStatsFileName = s },
                                                          "-nt" -> { s => noTransProb = s.toDouble },
                                                          "-cb" -> { s => commonBonus = s.toDouble })
                                                          
  val flags : Map[String, () => Unit] = Map("-a" -> { () => showRelativeAmp = true },
                                            "-d" -> { () => showDump = true } )  
  
  def processOptions(args : Array[String]) = {
    def isOption(s : String) = s.startsWith("-") && s.length > 1 && !s.charAt(1).isDigit 
    var i = 0
    var nonOptionCount = 0
    while (i < args.length) {
      val arg = args(i)
      if (isOption(arg)) {
        optionsWithVals.get(arg) match {
          case Some(func) => {
            if (i == args.length - 1 || isOption(args(i + 1)))
              throw new Exception(s"No value specified for option $arg")
            func(args(i + 1))
            i += 1
          }
          case None => flags.get(arg) match {
                         case Some(func) => func()
                         case None => throw new Exception(s"Unknown option: $arg")
                       }
        }
      }
      else {
        nonOptionCount += 1
        nonOptionCount match { 
          case 1 => inFileName = arg
          case 2 => outFileName = arg
          case _ => throw new Exception("This program does not take 3 arguments")
        }
      }
      i += 1
    }
  }
  
  def main(args : Array[String]) : Unit = {
    try {
      processOptions(args)
      val cd = new ChordDetector(inFileName = inFileName,
                                 outFileName = outFileName,
                                 windowExponent = windowExponent,
                                 sampleRate = sampleRate,
                                 maxWindows = maxWindows,
                                 showRelativeAmp = showRelativeAmp,
                                 showDump = showDump,
                                 shift = shift,
                                 virtualShift = virtualShift,
                                 step = step,
                                 meanCount = meanCount,
                                 bassShift = bassShift,
                                 octaves = octaves,
                                 learn = learn,
                                 inputStatsFileName = inputStatsFileName,
                                 noTransProb = noTransProb,
                                 commonBonus = commonBonus)
      cd.proceed
    }
    catch {
      case e : Exception => { println(s"Error: $e")
        e.printStackTrace
      }
    }
  }
}
