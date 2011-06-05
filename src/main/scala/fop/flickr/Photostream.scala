package fop
package flickr

import scalaz._, Scalaz._

case class Photostream(
  private val photostream: Set[Photo] = Set()
)

object Photostream {
  val lens = Lens[Photostream,Set[Photo]](
    get = a => a.photostream
  , set = (a,b) => a.copy(photostream = b)
  )
  val mutator = Lens.setLikeLens[Photostream,Photo,Set[Photo]](lens)
}
