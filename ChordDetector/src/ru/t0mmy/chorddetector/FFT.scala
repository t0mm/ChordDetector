package ru.t0mmy.chorddetector

import scala.language.reflectiveCalls

object FFT {
  import Complex._
  import math._
  
  def transform(data : Array[Complex]) : Unit = {
    val n = data.length
    if (n > 1) {
      def halfArray = Array.fill(n / 2)(Complex.zero) 
      val data0 = halfArray
      val data1 = halfArray
      for (i <- 0 until n / 2) {
        data0(i) = data(2 * i) 
        data1(i) = data(2 * i + 1) 
      }

      transform(data0)
      transform(data1)
      
      val angle = 2 * Pi / n
      val wn = cos(angle) + sin(angle).i
      var w = Complex.one
      for (i <- 0 until n / 2) {
        data(i) = data0(i) + w * data1(i)
        data(i + n / 2) = data0(i) - w * data1(i)
        w *= wn
      }
    }
  }
  
  def transform(data : Array[Double]) : Array[Double] = {
    val complexData = data.map(Complex(_))
    transform(complexData)
    complexData.map(_.abs)
  }
}
