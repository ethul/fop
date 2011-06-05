package fop
package http

import scalaz._, effects._, Scalaz._
import org.scalacheck._
import Prop.forAll
import java.io.{InputStream,FileInputStream}

object IterateesSpecification extends Properties("Iteratees") {
  // TODO: ethul, make this a real test
  property("http response") = forAll { (a: String) =>
    io { new FileInputStream("/tmp/iteratee2") }.bracket((fd: InputStream) => io(fd.close)) { 
      Enumerators.byteStream(_)(new Array[Byte](9))(Iteratees.httpResponse)
    } unsafePerformIO match {
      case IterV.Done(a,_) => { println("["+a._1+"]["+a._2+"]["+a._3+"]") ; false }
      case _ => false
    }
  }
}
