package ru.t0mmy.chorddetector

import scala.language.implicitConversions

case class Complex(re : Double, im : Double = 0) {
  def unary_- = new Complex(-re, -im)
  def + (x : Complex) = new Complex(re + x.re, im + x.im)
  def - (x : Complex) = new Complex(re - x.re, im - x.im)
  def * (x : Complex) = new Complex(re * x.re - im * x.im, re * x.im + im * x.re)
  
  def / (x : Complex) = {
    val d = -x.re * x.re - x.im * x.im
    new Complex((-re * x.re - im * x.im) / d, (im * x.re - re * x.im) / d)
  }
  
  def abs = math.sqrt(re * re + im * im)
  
  override def toString = if (im > 0) re.toString + " + " + im.toString + "i" else re.toString;
}

object Complex {
  val zero = Complex(0)
  val one = Complex(1)
  val i = Complex(0, 1)
  
  implicit def double2complex(x : Double) = new Complex(x, 0)
  implicit def doubleToImPart(x : Double) = new {
    def i = new Complex(0, x)
  }
}
