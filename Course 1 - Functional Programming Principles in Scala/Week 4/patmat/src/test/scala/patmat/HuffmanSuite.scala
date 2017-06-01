package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) ===
                   List(
                       Fork(
                           Leaf('e',1),
                           Leaf('t',2),
                           List('e', 't'),
                           3
                       ),
                       Leaf('x',4)
                   )
          )
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  val aLeaf: CodeTree = Leaf(char='a', 8)
  val bLeaf: CodeTree = Leaf(char='b', 3)
  val cLeaf: CodeTree = Leaf(char='c', 1)
  val dLeaf: CodeTree = Leaf(char='d', 1)
  val eLeaf: CodeTree = Leaf(char='e', 1)
  val fLeaf: CodeTree = Leaf(char='f', 1)
  val gLeaf: CodeTree = Leaf(char='g', 1)
  val hLeaf: CodeTree = Leaf(char='h', 1)

  val ghFork: CodeTree = Fork(gLeaf, hLeaf, chars=List('g', 'h'), weight=2)
  val efFork: CodeTree = Fork(eLeaf, fLeaf, chars=List('e', 'f'), weight=2)
  val cdFork: CodeTree = Fork(cLeaf, dLeaf, chars=List('c', 'd'), weight=2)

  val efghFork: CodeTree = Fork(efFork, ghFork, chars=List('e', 'f', 'g', 'h'), weight=4)
  val bcdFork: CodeTree  = Fork(bLeaf, cdFork, chars=List('b', 'c', 'd'), weight=5)

  val bcdefgh: CodeTree  = Fork(bcdFork, efghFork, chars=List('b', 'c', 'd', 'e', 'f', 'g', 'h'), weight=9)

  val fullCodeTree: CodeTree = Fork(aLeaf, bcdefgh, chars=List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'), weight=17)

  test("encode simple string") {
      val testString: List[Char] = List('b', 'a', 'c')
      val encodedString: List[Bit] = encode(fullCodeTree)(testString)

      val solution: List[Bit] = List(1, 0, 0, 0, 1, 0, 1, 0)
      assert(encodedString === solution)
  }

  test("decode single letter") {
      val encoding: List[Bit] = List(0)
      val decodedString: List[Char] = decode(fullCodeTree, encoding)
  }


  test("decode simple string") {
      val encoding: List[Bit] = List(1, 0, 0, 0, 1, 0, 1, 0)
      val decodedString: List[Char] = decode(fullCodeTree, encoding)

      val solution: List[Char] = List('b', 'a', 'c')

      assert(decodedString == solution)
  }

  test("create code tree from single letter") {
      val text = List('a')
      val codeTreeFromText = createCodeTree(text)

      val solution = Leaf('a', 1)

      assert( codeTreeFromText == solution )
  }




  test("encode and decode single letter") {
      val text = List('a')

      val codeTree: CodeTree = createCodeTree(text)
      val encodedSequence: List[Bit]  = encode(codeTree)(text)
      val decodedSequence: List[Char] = decode(codeTree, encodedSequence)

      assert(decodedSequence == text)
  }

  // test("create code tree from letters") {
  //     val text = "aaaaaaaabbbcdefgh".toList
  //     val codeTree: CodeTree = createCodeTree(text)
  //
  //     assert(codeTree == fullCodeTree)
  // }

  // test("encode and decode sequence") {
  //     val text = "abc".toList
  //   //   val text = List('l', ',', 'e', 'r', 'a', ' ', 'r', 'r', 'e', ' ', 'e', 'o', ' ', 'm', ' ', '4', '5', ' ', 'B', 'C', ',', ' ', 'l', 'p', 's', 't', 'i', 't', ',', ' ', 'o', 'e', 'i', 'l', 't', 'e', 'e', 'l', 'r', 'c', 'e', 's', 'e', 'l', 'p', ' ', ' ', 'o', 'l', 'o', 'a')
  //
  //     val codeTree: CodeTree = createCodeTree(text)
  //     val encodedSequence: List[Bit]  = encode(codeTree)(text)
  //     val decodedSequence: List[Char] = decode(codeTree, encodedSequence)
  //
  //     assert(decodedSequence == text)
  // }

}
