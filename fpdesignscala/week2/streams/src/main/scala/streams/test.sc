def streamNumbers(to: Int): Stream[Int] = {
  def iter(n: Int): Stream[Int] = {
    println("n = " + n)
    if (n < to)
      n #:: iter(n + 1)
    else Stream.empty
  }

  iter(0)

}


val stream = streamNumbers(100)

val stream10 = stream(2)
