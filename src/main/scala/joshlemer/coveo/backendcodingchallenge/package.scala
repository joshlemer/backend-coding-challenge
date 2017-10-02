package joshlemer.coveo

package object backendcodingchallenge {

  /** Useful for lifting types to a higher kind */
  type Id[+A] = A

  /** Useful for avoiding namespace pollution :-) */
  implicit class AnyOps[T](val t: T) extends AnyVal {
    def |>[TT](f: T => TT): TT = f(t)
  }

}
