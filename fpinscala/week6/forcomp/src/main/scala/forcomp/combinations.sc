val l = List(('a', 1), ('b', 2), ('c', 3))

('c', 3)

for {
  i <- 1 to l.length
  c <- l.combinations(i)
} yield c


def occurrenceCombinators(occurences: List[(Char, Int)]): List[List[(Char, Int)]] = {
  def combine(char: Char, count: Int): List[(Char, Int)] = {
    val combined = for {
      i <- 1 to count
    } yield (char, i)
    combined.toList
  }

  occurences.map(t => combine(t._1, t._2))
}

val l2 = List(('a', 1), ('b', 2))

val c1 = l2.combinations(2).toList

occurrenceCombinators(List(('c', 3), ('b', 2)))
