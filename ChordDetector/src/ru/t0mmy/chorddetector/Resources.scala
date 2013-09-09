package ru.t0mmy.chorddetector

import language.{reflectiveCalls, implicitConversions}

object Resources {
  type Closeable = { def close : Unit }

  class Resource[C](c : C) {
    private def exec[A](f : C => A) = {
      try
        f(c)
      catch {
        case e : Throwable => try {
                                c.asInstanceOf[Closeable].close
                                throw e 
                              }
                              catch {
                                case _ : Throwable => throw e
                              }
      }
      finally
        c.asInstanceOf[Closeable].close 
    }

    def flatMap[A](f : C => A) = exec(f)
    def map[A](f : C => A) = exec(f)
    def foreach(f : C => Unit) = exec(f)
  }

  implicit def closeable2resource[C <: java.io.Closeable](c : C) = new Resource(c) 
  implicit def connection2resource[C <: java.sql.Connection](c : C) = new Resource(c) 
  implicit def statement2resource[C <: java.sql.Statement](c : C) = new Resource(c) 
  implicit def resultSet2resource[C <: java.sql.ResultSet](c : C) = new Resource(c) 
}
