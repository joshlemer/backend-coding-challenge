package joshlemer.coveo

package object backendcodingchallenge {

  type Id[+A] = A

  implicit class AnyOps[T](val t: T) extends AnyVal {
    def |>[TT](f: T => TT): TT = f(t)
  }

}
