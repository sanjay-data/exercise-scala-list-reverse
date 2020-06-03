import TailRecursiveAlgos._
import org.scalatest._
import org.scalatest.matchers.should.Matchers._

class TailRecursiveAlgosSpec extends org.scalatest.funspec.AnyFunSpecLike {

  describe("reverseList") {

    it("should return empty list if is empty") {
      reverseList(List()) shouldBe empty
    }

    it("should return a list with elements reversed") {
      reverseList(List(1, 2)) should contain theSameElementsAs List(2, 1)
      reverseList(List(1, 2, 3, 4, 5)) should contain theSameElementsAs List(5, 4, 3, 2, 1)
      reverseList(List("Hello", "World")) should contain theSameElementsAs List("World", "Hello")
      reverseList("Apple".toList) should contain theSameElementsAs "Apple".reverse.toList
    }

  }

  describe("shorten") {

    it(
      "should replace repeated sequence of chars with number and the char repeated"
    ) {
      shorten("AAAAA") shouldBe "5A"
      shorten("AAABBCCC") shouldBe "3A2B3C"
    }

    it("should not replace sequence containing char repeated once") {
      shorten("A") shouldBe "A"
      shorten("AAB") shouldBe "2AB"
      shorten("ABB") shouldBe "A2B"
      shorten("AACBB") shouldBe "2AC2B"
    }
  }

  describe("expand") {

    it("should expand the char number of times it is present") {
      expand("5A") shouldBe "AAAAA"
      expand("3A2B3C") shouldBe "AAABBCCC"
      expand("A") shouldBe "A"
      expand("2AB") shouldBe "AAB"
      expand("A2B") shouldBe "ABB"
      expand("2C2B") shouldBe "CCBB"
    }
  }

  describe("expand & shorten") {

    it("should give the same string back with output of shorten expanded") {
      expand(shorten("AAAAA")) shouldBe "AAAAA"
      expand(shorten("AAABBCCC")) shouldBe "AAABBCCC"
      expand(shorten("A")) shouldBe "A"
      expand(shorten("AAB")) shouldBe "AAB"
      expand(shorten("ABB")) shouldBe "ABB"
      expand(shorten("CCBB")) shouldBe "CCBB"
    }
  }
}
