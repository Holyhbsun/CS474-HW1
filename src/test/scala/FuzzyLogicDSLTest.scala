import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import FuzzyLogicDSL._
import FuzzySetOperations._

class FuzzyLogicDSLTest extends AnyFlatSpec with Matchers {

  behavior of "FuzzyLogicDSL"

  it should "correctly evaluate ADD operation" in {
    val gate = FuzzyGate("addGate", ADD(Input("A"), Input("B")))
    Assign(gate)

    Scope(gate, (Input("A"), 0.4))
    Scope(gate, (Input("B"), 0.5))

    TestGate("addGate", expectedResult = 0.9) shouldBe true
  }

  it should "correctly evaluate MULT operation" in {
    val gate = FuzzyGate("multGate", MULT(Input("A"), Input("B")))
    Assign(gate)

    Scope(gate, (Input("A"), 0.6))
    Scope(gate, (Input("B"), 0.5))

    TestGate("multGate", expectedResult = 0.3) coshouldBe true
  }

  it should "correctly evaluate XOR operation" in {
    val gate = FuzzyGate("xorGate", XOR(Input("A"), Input("B")))
    Assign(gate)

    Scope(gate, (Input("A"), 0.7))
    Scope(gate, (Input("B"), 0.4))
    TestGate("xorGate", expectedResult = 0.3) shouldBe true
  }

  it should "correctly evaluate composite gate with nested operations" in {
    val gate = FuzzyGate("compositeGate", ADD(MULT(Input("A"), Input("B")), Input("C")))
    Assign(gate)

    Scope(gate, (Input("A"), 0.5))
    Scope(gate, (Input("B"), 0.7))
    Scope(gate, (Input("C"), 0.2))

    TestGate("compositeGate", expectedResult = 0.55) shouldBe true
  }

  it should "correctly throw an error when input is missing" in {
    val gate = FuzzyGate("errorGate", ADD(Input("A"), Input("B")))
    Assign(gate)

    Scope(gate, (Input("A"), 0.5))

    intercept[IllegalArgumentException] {
      TestGate("errorGate", expectedResult = 0.5)
    }
  }

  it should "correctly handle input overrides and value cap" in {
    val gate = FuzzyGate("overrideGate", ADD(Input("A"), Input("B")))
    Assign(gate)

    Scope(gate, (Input("A"), 0.3))
    Scope(gate, (Input("B"), 0.6))

    TestGate("overrideGate", expectedResult = 0.9) shouldBe true
    Scope(gate, (Input("A"), 0.5))
    TestGate("overrideGate", expectedResult = 1.0) shouldBe true
  }

  behavior of "FuzzySet operations"

  it should "correctly evaluate XOR for two fuzzy sets" in {
    val setA = FuzzySet("A", List(Element("x1", 0.2), Element("x2", 0.7), Element("x3", 0.5)))
    val setB = FuzzySet("B", List(Element("x1", 0.6), Element("x2", 0.3), Element("x3", 0.5)))

    val xorResult = xor(setA, setB).eval()

    xorResult shouldBe List(Element("x1", 0.4), Element("x2", 0.4), Element("x3", 0.0))
  }

  it should "correctly evaluate ADD for two fuzzy sets" in {
    val setA = FuzzySet("A", List(Element("x1", 0.2), Element("x2", 0.7), Element("x3", 0.5)))
    val setB = FuzzySet("B", List(Element("x1", 0.6), Element("x2", 0.3), Element("x3", 0.5)))

    val addResult = add(setA, setB).eval()

    addResult shouldBe List(Element("x1", 0.8), Element("x2", 1.0), Element("x3", 1.0))
  }

  it should "correctly evaluate MULT for two fuzzy sets" in {
    val setA = FuzzySet("A", List(Element("x1", 0.2), Element("x2", 0.7), Element("x3", 0.5)))
    val setB = FuzzySet("B", List(Element("x1", 0.6), Element("x2", 0.3), Element("x3", 0.5)))

    val multResult = mult(setA, setB).eval()

    multResult shouldBe List(Element("x1", 0.12), Element("x2", 0.21), Element("x3", 0.25))
  }

  it should "correctly evaluate UNION for two fuzzy sets" in {
    val setA = FuzzySet("A", List(Element("x1", 0.2), Element("x2", 0.7), Element("x3", 0.5)))
    val setB = FuzzySet("B", List(Element("x1", 0.6), Element("x2", 0.3), Element("x3", 0.5)))

    val unionResult = union(setA, setB).eval()

    unionResult shouldBe List(Element("x1", 0.6), Element("x2", 0.7), Element("x3", 0.5))
  }

  it should "correctly evaluate INTERSECTION for two fuzzy sets" in {
    val setA = FuzzySet("A", List(Element("x1", 0.2), Element("x2", 0.7), Element("x3", 0.5)))
    val setB = FuzzySet("B", List(Element("x1", 0.6), Element("x2", 0.3), Element("x3", 0.5)))

    val intersectionResult = intersection(setA, setB).eval()

    intersectionResult shouldBe List(Element("x1", 0.2), Element("x2", 0.3), Element("x3", 0.5))
  }

  it should "correctly evaluate COMPLEMENT for a fuzzy set" in {
    val setA = FuzzySet("A", List(Element("x1", 0.2), Element("x2", 0.7), Element("x3", 0.5)))

    val complementResult = complement(setA).eval()

    complementResult shouldBe List(Element("x1", 0.8), Element("x2", 0.3), Element("x3", 0.5))
  }

  it should "correctly evaluate the α-cut(0.6) for a fuzzy set" in {
    val setA = FuzzySet("A", List(Element("x1", 0.3), Element("x2", 0.7), Element("x3", 0.8)))

    val alphaCutResult = alphaCut(setA, 0.6)

    alphaCutResult shouldBe List("x2", "x3")
  }
}