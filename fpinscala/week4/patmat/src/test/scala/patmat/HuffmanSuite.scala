package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("weight of a larger larger tree") {
    new TestTrees {
      assert(weight(t2) === 9)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("chars repetition of a larger tree") {
    new TestTrees {
      assert(times(List('a', 'b', 'a')) === List(('a', 2), ('b', 1)))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("check all code trees are leafs") {
    val leaves = List(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5))
    assert(singleton(leaves))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("create huffman tree") {
    val text: String = "Vaffammoc a mamt"

    val codeTree = createCodeTree(text)
    codeTree match {
      case Fork(_, _, chars, weight) =>
        assert(weight === text.length)
    }
  }

  test("encode simple string code tree") {
    val text: List[Char] = "aaaaaabbbcc".toList
    val tree:CodeTree = createCodeTree(text)

    // assert(chars(tree) === List('a', 'b', 'c'))
    assert(weight(tree) === text.length)

    //val encoded: List[Bit] = encode(t1)(text)
    //assert(decode(t1, encoded) === text)

  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      val chars: List[Char] = "ab".toList
      val encoded: List[Bit] = encode(t1)(chars)
      assert(decode(t1, encoded) === chars)
    }
  }

  test("code table decode and encode a very short text should be identity") {
    new TestTrees {
      val chars: List[Char] = "ab".toList
      val encoded: List[Bit] = quickEncode(t1)(chars)
      assert(decode(t1, encoded) === chars)
    }
  }

}
