package fop
package http

import scalaz._, scalaz.effects._, Scalaz._, IterV._
import java.io.{InputStream,FileInputStream}

object Enumerators {
  def byteStream(fd: InputStream)(buffer: Chunk): EnumeratorM[IO,Chunk] = new EnumeratorM[IO,Chunk] {
    def apply[A](it: IterV[Chunk,A]) = {
      import scala.annotation.tailrec
      @tailrec def loop(ii: IterV[Chunk,A]): IO[IterV[Chunk,A]] = ii match {
        case i@Done(_,_) => io { i }
        case i@Cont(k) => { 
          // TODO: ethul, not sure yet how to make this monadic and retain tailrec
          val a = fd.read(buffer)
          a match {
            case -1 => io { k(EOF[Chunk]) }
            case x if x < buffer.length => loop(k(El(buffer take x)))
            case _ => loop(k(El(buffer)))
          }
        } 
      }
      loop(it)
    }
  }
}
