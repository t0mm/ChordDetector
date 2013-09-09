package ru.t0mmy.chorddetector

class KeyShifter(notes : Array[String], shiftValue : Int) {
  def shift(chord : String) =
    if (shiftValue == 0)
      chord
    else {
      def posMod12(n : Int) = {
        val m = n % 12
        if (m < 0) m + 12 else m
      }
      val noteLen = if (chord.length > 1 && chord.charAt(1) == '#') 2 else 1
      val note = chord.substring(0, noteLen)
      val suffix = chord.substring(noteLen)
      val idx = notes.indexOf(note)
      val newIdx = posMod12(idx + shiftValue)
      val newNote = notes(newIdx)
      newNote + suffix
  }
}
