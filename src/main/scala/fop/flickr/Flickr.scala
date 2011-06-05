package fop
package flickr

import scalaz._, Scalaz._

/**
 * representation of flickr which is interpreted as a large context (or state)
 * where the state is modified through an appropriate lens. for example, to
 * upload a new photo to the api, we want to compose a lens which sets the
 * photostream's fiels appropriately with a lens which sets the photostream
 * in this flickr class
 */
case class Flickr(
  private val authentication: Authentication = Authentication()
, private val photostream: Photostream = Photostream()
)

object Flickr {
  val lens = Lens[Flickr,Photostream](
    get = a => a.photostream
  , set = (a,b) => a.copy(photostream = b)
  )

  // this lens knows how to create a request to the flickr api
  // to do something with the photostream, but it needs to be
  // composed with the lens that can populate the photostream
  val apiLens = Lens[Flickr,Photostream](
    get = a => {
      println("contacting api...")
      a.photostream
    }
  , set = (a,b) => {
      println("contacting api...")
      a.copy(photostream = b)
    }
  )
}
