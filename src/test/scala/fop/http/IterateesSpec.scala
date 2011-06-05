package fop
package http

import scalaz._, effects._, Scalaz._
import org.scalacheck._
import Prop.forAll
import java.io.InputStream

object IterateesSpecification extends Properties("Iteratees") {
  /**
   * tests a raw http response, which really just means how the status line,
   * headers, and body are organized in the input stream. this is not testing
   * the actual content within each segment
   */
  property("raw http response structure") = forAll(
    Gen.alphaStr suchThat (!_.contains("\n"))
    , Gen.alphaStr suchThat (!_.contains("\n\n"))
    , Gen.alphaStr
    , Gen.choose(1,10)
  ) { (a,b,c,d) =>
    import java.io.ByteArrayInputStream
    val input = new ByteArrayInputStream((a+"\n"+b+"\n\n"+c).toArray map (_.toByte))
    io { input }.bracket((in: InputStream) => io { in.close }) { 
      Enumerators.byteStream(_)(new Array[Byte](d))(Iteratees.httpResponse)
    } unsafePerformIO match {
      case IterV.Done(x,_) => { 
        a == x._1 &&
        b == x._2 &&
        c == x._3
      }
      case _ => false
    }
  }
}
