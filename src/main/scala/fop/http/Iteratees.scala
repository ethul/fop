package fop
package http

import scalaz._, scalaz.effects._, Scalaz._, IterV._

object Iteratees {
  import Utils._

  val getStatusLine = getBytesUntilSeq(Array('\n'.toByte))

  val getHeaders = getBytesUntilSeq(Array('\n'.toByte,'\n'.toByte))

  val getPayload: IterV[Array[Byte],String] = {
    def step(acc: StringBuilder)(s: Input[Array[Byte]]): IterV[Array[Byte],String] = {
      s(el = e => Cont(step(acc appendAll e.map(_.toChar)))
      , empty = Cont(step(acc))
      , eof = Done(acc toString, EOF[Array[Byte]])
      )
    }
    Cont(step(new StringBuilder))
  }

  val httpResponse = for {
    a <- getStatusLine
    b <- getHeaders
    c <- getPayload
  } yield (a,b,c)

  private object Utils {
    val getBytesUntilSeq: Seq[Byte] => IterV[Array[Byte],String] = seq => {
      def step(acc: StringBuilder)(s: Input[Array[Byte]]): IterV[Array[Byte],String] = {
        s(el = e => {
            val i = e.indexOfSlice(seq)
            if (i == -1) Cont(step(acc appendAll e.map(_.toChar)))
            else Done({ acc appendAll e.take(i).map(_.toChar) ; acc toString }, El(e drop i))
          }
        , empty = Cont(step(acc))
        , eof = Done(acc toString, EOF[Array[Byte]])
        )
      }
      Cont(step(new StringBuilder))
    }
  }
}
