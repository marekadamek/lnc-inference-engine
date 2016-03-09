
package object nclogic {
  implicit class Iff[T](val b: T) {
    def :>[B](f: T => B):B = f(b)
  }
}
