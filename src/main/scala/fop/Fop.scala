package fop

import scalaz._, scalaz.effects._, Scalaz._

object Fop {
  def main(args: Array[String]): Unit = run
  def run: Unit = {
    println("fop your photos!")

    import flickr._
    val m = Photostream.mutator
    val r =
      for {
        _ <- m += "c.jpg"
        z <- m += "d.jpg"
      } yield z 

    //val x = Flickr.apiLens.set(Flickr(), r ~> Photostream(Set("a.jpg", "b.jpg")))
    val x = (Photostream.lens compose Flickr.apiLens).set(Flickr(), r ! Photostream(photostream = Set("g.jpg")))
    println(x)

    // add each photo to upload to the photostream object using a wrapped lens
    // add the photostream to the flickr object using a lens
    // flickr has an api lens which knows how to set the photostream on your account
    // flickr could also have an authentication lens

    val s =
      for {
        a <- IO(w => (w,"happy"))
        b <- IO(w => (w,"brave"))
        c <- IO(w => (w,"goose"))
      } yield a + b + c

    val i = IO(w => (w,"happy"))
    val ii = Foo.lens.set(Foo(bar = i), s)
    println(Foo.lens.get(ii).unsafePerformIO)
  }
}

case class Foo(bar: IO[String])
object Foo {
  val lens = Lens[Foo,IO[String]](get = foo => foo.bar, set = (foo,b) => foo.copy(bar = b))
}
