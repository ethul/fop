package fop
package http

import scalaz._, scalaz.effects._, Scalaz._, IterV._
import java.io.{InputStream,FileInputStream}

object Enumerators {
  def byteStream(fd: InputStream)(buffer: Array[Byte]): EnumeratorM[IO,Array[Byte]] = new EnumeratorM[IO,Array[Byte]] {
    def apply[A](it: IterV[Array[Byte],A]) = {
      import scala.annotation.tailrec
      @tailrec def loop(ii: IterV[Array[Byte],A]): IO[IterV[Array[Byte],A]] = ii match {
        case i@Done(_,_) => io { i }
        case i@Cont(k) => { 
          val a = fd.read(buffer)
          a match {
            case -1 => io { k(EOF[Array[Byte]]) }
            case x if x < buffer.length => loop(k(El(buffer take x)))
            case _ => loop(k(El(buffer)))
          }
        } 
      }
      loop(it)
    }
  }
}
