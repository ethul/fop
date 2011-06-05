package fop
package http

import scalaz._, scalaz.effects._, Scalaz._, IterV._

object Iteratees {
  import Utils._

  def getStatusLine: IterV[Chunk,String] = getBytesUntilSep("\n")

  def getHeaders: IterV[Chunk,String] = getBytesUntilSep("\n\n")

  def getPayload: IterV[Chunk,String] = {
    def step(acc: StringBuilder)(s: Input[Chunk]): IterV[Chunk,String] = {
      s(el = e => Cont(step(acc appendAll e.map(_.toChar)))
      , empty = Cont(step(acc))
      , eof = Done(acc toString, EOF[Chunk])
      )
    }
    Cont(step(new StringBuilder))
  }

  def popEol: IterV[Chunk,Unit] = {
    def step(s: Input[Chunk]): IterV[Chunk,Unit] = {
      s(el = e => {
          if (e.indexOf('\n'.toByte) == 0) Done((), El(e tail))
          else Done((), s)
        }
      , empty = Cont(step)
      , eof = Done((), EOF[Chunk])
      )
    }
    Cont(step)
  }

  def httpResponse: IterV[Chunk,(String,String,String)] = {
    for {
      a <- getStatusLine
      _ <- popEol
      b <- getHeaders
      _ <- popEol
      _ <- popEol
      c <- getPayload
    } yield (a,b,c)
  }

  private object Utils {
    def getBytesUntilSep: String => IterV[Chunk,String] = sep => {
      def step(acc: StringBuilder)(s: Input[Chunk]): IterV[Chunk,String] = {
        s(el = e => {
            // we have to handle the case when part of the sep is already in the acc
            // and the other part is in the chunk. so we add the chunk right away and
            // then if the sep is in fact inside, we pull it out along with the chunk
            // part following the sep and pass it along as unprocessed
            val acc1 = acc.appendAll(e.map(_.toChar))
            val i = acc1.indexOf(sep)
            if (i != -1) {
              val res = acc1.toString
              val left = res.slice(0, i)
              val right = res.slice(i, res.length)
              Done(left, El(right.toArray.map(_.toByte)))
            }
            else Cont(step(acc1))
          }
        , empty = Cont(step(acc))
        , eof = Done(acc toString, EOF[Chunk])
        )
      }
      Cont(step(new StringBuilder))
    }
  }
}
