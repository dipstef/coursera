
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range.
    *
    * This ensure that the value of x and y coordinates are confined within the dimensions
    * */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))

    def apply(x: Int, y: Int): RGBA = data(y * width + x)

    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel_(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // as requested from the assignment solution was to use two while loops but we can do  better than this

    if (radius == 0)
      src(x, y)
    else {
      var xRadius = clamp(x - radius, 0, src.width - 1)
      var (r, g, b, a) = (0, 0, 0, 0)
      var pixels = 0

      while (xRadius <= clamp(x + radius, 0, src.width - 1)) {
        var yRadius = clamp(y - radius, 0, src.height - 1)

        while (yRadius <= clamp(y + radius, 0, src.height - 1)) {
          val pixel = src(xRadius, yRadius)

          r += red(pixel)
          g += green(pixel)
          b += blue(pixel)
          a += alpha(pixel)

          yRadius += 1
          pixels += 1
        }
        xRadius += 1
      }

      rgba(r / pixels, g / pixels, b / pixels, a / pixels)
    }
  }

  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // as requested from the assignment solution was to use two while loops but we can do much better than this

    if (radius > 0) {
      val pixels = for {
        xRadius <- clamp(x - radius, 0, src.width - 1) to clamp(x + radius, 0, src.width - 1)
        yRadius <- clamp(y - radius, 0, src.height - 1) to clamp(y + radius, 0, src.height - 1)
        pixel = src(xRadius, yRadius)
      } yield (red(pixel), green(pixel), blue(pixel), alpha(pixel))


      val (rs, gs, bs, as) = pixels.reduce {
        (t1, t2) => (t1._1 + t2._1, t1._2 + t2._2, t1._3 + t2._3, t1._4 + t2._4)
      }

      rgba(rs / pixels.length, gs / pixels.length, bs / pixels.length, as / pixels.length)
    } else {
      src(x, y)
    }

  }


}
