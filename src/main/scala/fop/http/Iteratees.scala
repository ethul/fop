package fop
package http

import scalaz._, scalaz.effects._, Scalaz._, IterV._

object Iteratees {
  import Utils._

  object Http {
    /**
     * response = *CRLF Status-Line *(message-header CRLF) CRLF [ message-body ]
     */
    def response: IterV[Chunk,(String,String,String)] = {
      for {
        _ <- skip("\n")
        a <- line 
        b <- linesUntilEmptyLine
        _ <- newline
        c <- lines
      } yield (a,b,c)
    }
  }

  private object Utils {
    def line: IterV[Chunk,String] = {
      def step(acc: StringBuilder)(s: Input[Chunk]): IterV[Chunk,String] = {
        s(el = e => {
            // we have to handle the case when part of the sep is already in the acc
            // and the other part is in the chunk. so we add the chunk right away and
            // then if the sep is in fact inside, we pull it out along with the chunk
            // part following the sep and pass it along as unprocessed
            val acc1 = acc.appendAll(e.map(_.toChar))
            val i = acc1.indexOf("\n")
            if (i != -1) {
              val res = acc1.toString
              val left = res.slice(0, i+1)
              val right = if (i == res.length-1) "" else res.slice(i+1, res.length)
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

    def lines: IterV[Chunk,String] = {
      def step(acc: StringBuilder)(s: Input[Chunk]): IterV[Chunk,String] = {
        s(el = e => Cont(step(acc appendAll e.map(_.toChar)))
        , empty = Cont(step(acc))
        , eof = Done(acc toString, EOF[Chunk])
        )
      }
      Cont(step(new StringBuilder))
    }

    // TODO: ethul, this iteratee should reuse peek and readLine
    def linesUntilEmptyLine: IterV[Chunk,String] = {
      def step(acc: StringBuilder)(s: Input[Chunk]): IterV[Chunk,String] = {
        s(el = e => {
            if (acc.isEmpty && e.indexOf('\n'.toByte) == 0) Done(acc toString, s)
            else {
              val acc1 = acc.appendAll(e map (_ toChar))
              val i = acc1.indexOf("\n\n")
              if (i == -1) Cont(step(acc1))
              else {
                val res = acc1.toString
                val left = res.slice(0, i+1)
                val right = res.slice(i+1, res.length)
                Done(left, El(right.toArray map (_ toByte)))
              }
            }
          }
        , empty = Cont(step(acc))
        , eof = Done(acc toString, EOF[Chunk])
        )
      }
      Cont(step(new StringBuilder))
    }

    def newline: IterV[Chunk,String] = {
      def step(s: Input[Chunk]): IterV[Chunk,String] = {
        s(el = e => {
            if (e.indexOf('\n'.toByte) == 0) Done("\n", El(e tail))
            else Done("", s)
          }
        , empty = Cont(step)
        , eof = Done("", EOF[Chunk])
        )
      }
      Cont(step)
    }

    def skip: String => IterV[Chunk,Unit] = {
      sk => {
        import scala.annotation.tailrec
        @tailrec def loop(acc: StringBuilder): StringBuilder = {
          if (acc.indexOf(sk) == 0) loop(acc.delete(0,sk length))
          else acc
        }
        def step(acc: StringBuilder)(s: Input[Chunk]): IterV[Chunk,Unit] = {
          s(el = e => {
              acc.appendAll(e map (_ toChar))
              if (acc.length < sk.length) Cont(step(acc))
              else if (acc.indexOf(sk) == 0) Cont(step(loop(acc)))
              else Done((), El(acc.toString.toArray.map(_ toByte)))
            }
          , empty = Cont(step(acc))
          , eof = Done((), EOF[Chunk])
          )
        }
        Cont(step(new StringBuilder))
      }
    }
  }
}
