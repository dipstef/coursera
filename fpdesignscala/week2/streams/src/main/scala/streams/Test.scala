package streams

object Test extends App{

  def streamNumbers(to: Int): Stream[Int] = {
    def iter(n: Int): Stream[Int] = {
      println("n = " + n)
      if (n < to)
        n #:: iter(n + 1)
      else Stream.empty
    }

    iter(0)

  }

  def streamNumbersAcc(to: Int): Stream[Int] = {
    def iter(n: Int,  acc:Stream[Int]): Stream[Int] = {
      println("n = " + n)
      if (n < to)
        iter(n + 1, n #:: acc)
      else acc
    }

    iter(0, Stream.empty)

  }


  val stream = streamNumbers(100)
  val stream2 = streamNumbersAcc(100)


}
