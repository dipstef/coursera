package patmat

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite2 extends FunSuite {

  trait TestTrees {
    val l1 = Leaf('h', 1)
    val l2 = Leaf('h', 2)
    val t0 = Fork(Leaf('q', 1), Leaf('x', 1), List('q', 'x'), 2)
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight") {
    new TestTrees {
      assert(weight(l1) === 1)
      assert(weight(l2) === 2)
      assert(weight(t0) === 2)
      assert(weight(t1) === 5)
      assert(weight(t2) === 9)
    }
  }

  test("chars") {
    new TestTrees {
      assert(chars(l1) === List('h'))
      assert(chars(l2) === 'h' :: Nil)
      assert(chars(t0) === List('q', 'x'))
      assert(chars(t1) === List('a', 'b'))
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }



  test("makeOrderedLeafList") {
    assert(makeOrderedLeafList(Nil) === List())
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
    assert(makeOrderedLeafList(List(('t', 7))) === List(Leaf('t', 7)))
    assert(makeOrderedLeafList(List(('t', 9), ('w', 1), ('f', 2), ('k', 8))) === List(Leaf('w', 1), Leaf('f', 2), Leaf('k', 8), Leaf('t', 9)))
    assert(makeOrderedLeafList(List(('t', -1), ('e', -2), ('l', 0))) === List(Leaf('e', -2), Leaf('t', -1), Leaf('l', 0)))
  }

  test("singleton") {
    new TestTrees {
      assert(singleton(Nil) === false)
      assert(singleton(List(l1)) === true)
      assert(singleton(List(l1, l2)) === false)
    }
  }

  test("combine") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))

    assert(combine(Nil) === Nil)
    assert(combine(List(Leaf('f', 1))) === Leaf('f', 1) :: Nil)
    assert(combine(List(Leaf('f', 1), Leaf('g', 2))) === List(Fork(Leaf('f', 1), Leaf('g', 2), List('f', 'g'), 3)))
  }

  test("createCodeTree") {

    val sentence1: List[Char] = string2Chars("ciao,stefano.come stai bello?")
    val sentence2: List[Char] = string2Chars("ciao")
    val sentence3: List[Char] = string2Chars("erreo")

    val tree1 = createCodeTree(sentence1)
    val tree2 = createCodeTree(sentence2)
    val tree3 = createCodeTree(sentence3)

    assert(tree1.toString.contains("Leaf(o,4)"))
    assert(tree1.toString.contains("Leaf(a,3)"))
    assert(tree1.toString.contains("Leaf(?,1)"))

    assert(tree2.toString.split("Leaf").length === 5)

    assert(tree3.toString.contains("Leaf(e,2)"))
    assert(tree3.toString.contains("Leaf(r,2)"))
    assert(tree3.toString.contains("Leaf(o,1)"))
  }

  test("decode") {
    new TestTrees {
      assert(decode(t2, List(1, 0, 1, 0, 1, 2)) === Nil)
      assert(decode(t2, Nil) === Nil)
      assert(decode(t0, List(1, 1, 0, 0)) === List('x', 'x', 'q', 'q'))
      assert(decode(l1, List(0, 1, 1, 0)) === Nil)
    }
  }

  test("encode") {
    new TestTrees {
      assert(quickEncode(Leaf('d', 1))(Nil) === Nil)
      assert(quickEncode(t0)(List('a', 'b', 'c', 'd', 'e')) === Nil)
      assert(quickEncode(t1)(List('a', 'b', 'c', 'd', 'e')) === List(0, 1))
      assert(quickEncode(t2)(List('a', 'b', 'c', 'd', 'e')) === List(0, 0, 0, 1, 1))
    }
  }

  test("quickEncode") {
    new TestTrees {
      assert(quickEncode(Leaf('d', 1))(Nil) === Nil)
      assert(quickEncode(t0)(List('a', 'b', 'c', 'd', 'e')) === Nil)
      assert(quickEncode(t1)(List('a', 'b', 'c', 'd', 'e')) === List(0, 1))
      assert(quickEncode(t2)(List('a', 'b', 'c', 'd', 'e')) === List(0, 0, 0, 1, 1))
    }
  }

  test("decode & encode using identity !") {
    new TestTrees {
      assert(decode(t0, encode(t0)("qx".toList)) === "qx".toList)
      assert(decode(t0, encode(t0)("xq".toList)) === "xq".toList)

      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)

      assert(decode(t2, encode(t2)("adb".toList)) === "adb".toList)
    }
  }

  test("decode & quickEncode using identity !") {
    new TestTrees {
      assert(decode(t0, quickEncode(t0)("qx".toList)) === "qx".toList)
      assert(decode(t0, quickEncode(t0)("xq".toList)) === "xq".toList)

      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)

      assert(decode(t2, quickEncode(t2)("adb".toList)) === "adb".toList)
    }
  }

  test("mergeCodeTables") {
    val tabA = List(('a', List(1, 0, 1, 0)))
    val tabB = List(('b', List(0, 1, 0, 1)))
    val tabC = tabA ::: tabB

    assert(mergeCodeTables(Nil, Nil) === Nil)
    assert(mergeCodeTables(Nil, tabB) === tabB)
    assert(mergeCodeTables(tabA, Nil) === tabA)

    assert(mergeCodeTables(tabC, tabC).size === 4)
    assert(mergeCodeTables(tabC, tabB).head._2 === List(1, 0, 1, 0))
  }

  test("convert") {
    new TestTrees {
      assert(convert(Leaf('f', 1)) === Nil)

      val tableT0 = convert(t0)
      assert(tableT0.size === 2)
      assert(tableT0.head === ('q', List(0)))
      assert(tableT0.tail.head === ('x', List(1)))
    }
  }

  test("decode french secret") {
    assert(decode(frenchCode, secret) === string2Chars("huffmanestcool"))
  }

}
