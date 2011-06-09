package fop
package http

import scalaz._, effects._, Scalaz._
import org.scalacheck._
import Prop.forAll
import java.io.InputStream

object IterateesSpecification extends Properties("Iteratees") {
  import java.io.ByteArrayInputStream
  /**
   * tests an http response, which really just means how the status line,
   * headers, and body are organized in the input stream. this is not testing
   * the actual content within each segment
   */
  property("http response") = forAll(
    Gen.alphaStr suchThat (!_.isEmpty), Gen.listOf(Gen.alphaStr suchThat (!_.isEmpty)), Gen.alphaStr, Gen.choose(1,10), Gen.choose(0,5)) { 
    (statusLine,headers,body,chunkSize,leading) => {
      val newlines = (0 to leading).map(x => "\n").mkString
      val headerStr = if (headers.length == 0) "" else headers.mkString("\n")+"\n"
      val input = new ByteArrayInputStream((newlines+statusLine+"\n"+headerStr+"\n"+body).toArray map (_.toByte))
      val setup = io { input }.bracket((in: InputStream) => io { in.close }) { 
        Enumerators.byteStream(_)(new Array[Byte](chunkSize))(Iteratees.Http.response)
      } 
      setup.unsafePerformIO match {
        case IterV.Done(x,_) => { 
          statusLine+"\n" == x._1 &&
          headerStr == x._2 &&
          body == x._3
        }
        case _ => false
      }
    }
  }
}
