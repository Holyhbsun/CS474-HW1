sealed trait FuzzyGateOperation

case class Input(name: String, value: Option[Double] = None) extends FuzzyGateOperation
case class ADD(a: FuzzyGateOperation, b: FuzzyGateOperation) extends FuzzyGateOperation
case class MULT(a: FuzzyGateOperation, b: FuzzyGateOperation) extends FuzzyGateOperation
case class XOR(a: FuzzyGateOperation, b: FuzzyGateOperation) extends FuzzyGateOperation

case class FuzzyGate(name: String, operation: FuzzyGateOperation)

object FuzzyOperations {
  def round(value: Double, precision: Int = 2): Double = {
    val scale = Math.pow(10, precision)
    Math.round(value * scale) / scale
  }

  def add(a: Double, b: Double): Double = Math.min(1.0, round(a + b))
  def mult(a: Double, b: Double): Double = round(a * b)
  def xor(a: Double, b: Double): Double = round(Math.abs(a - b))
}

object FuzzyGateEvaluator {
  def evaluate(gate: FuzzyGate, inputs: Map[String, Double]): Double = {
    def evalOperation(op: FuzzyGateOperation): Double = op match {
      case Input(name, _) => inputs.getOrElse(name, throw new IllegalArgumentException(s"Input $name is not defined in the scope"))
      case ADD(a, b) => FuzzyOperations.add(evalOperation(a), evalOperation(b))
      case MULT(a, b) => FuzzyOperations.mult(evalOperation(a), evalOperation(b))
      case XOR(a, b) => FuzzyOperations.xor(evalOperation(a), evalOperation(b))
    }
    evalOperation(gate.operation)
  }
}

// Set Operations
case class Element(name: String, value: Double)

case class FuzzySet(name: String, elements: List[Element]) {
  def eval(): List[Element] = elements
}

object FuzzySetOperations {

  def round(value: Double, precision: Int = 2): Double = {
    val scale = Math.pow(10, precision)
    Math.round(value * scale) / scale
  }

  def elementMap(set: FuzzySet): Map[String, Double] = {
    set.elements.map(e => e.name -> e.value).toMap
  }

  def union(setA: FuzzySet, setB: FuzzySet): FuzzySet = {
    val mapA = elementMap(setA)
    val mapB = elementMap(setB)

    val allKeys = mapA.keySet ++ mapB.keySet
    val newElements = allKeys.map { key =>
      val valueA = mapA.getOrElse(key, 0.0)
      val valueB = mapB.getOrElse(key, 0.0)
      Element(key, round(Math.max(valueA, valueB)))
    }.toList

    FuzzySet(s"${setA.name}_UNION_${setB.name}", newElements)
  }

  def intersection(setA: FuzzySet, setB: FuzzySet): FuzzySet = {
    val mapA = elementMap(setA)
    val mapB = elementMap(setB)

    val allKeys = mapA.keySet ++ mapB.keySet
    val newElements = allKeys.map { key =>
      val valueA = mapA.getOrElse(key, 0.0)
      val valueB = mapB.getOrElse(key, 0.0)
      Element(key, round(Math.min(valueA, valueB)))
    }.toList

    FuzzySet(s"${setA.name}_INTERSECTION_${setB.name}", newElements)
  }

  def complement(setA: FuzzySet): FuzzySet = {
    val newElements = setA.elements.map {
      case Element(nameA, valueA) =>
        Element(nameA, round(1 - valueA))
    }
    FuzzySet(s"${setA.name}_COMPLEMENT", newElements)
  }

  def add(setA: FuzzySet, setB: FuzzySet): FuzzySet = {
    val mapA = elementMap(setA)
    val mapB = elementMap(setB)

    val allKeys = mapA.keySet ++ mapB.keySet
    val newElements = allKeys.map { key =>
      val valueA = mapA.getOrElse(key, 0.0)
      val valueB = mapB.getOrElse(key, 0.0)
      Element(key, round(Math.min(1.0, valueA + valueB)))
    }.toList

    FuzzySet(s"${setA.name}_ADD_${setB.name}", newElements)
  }

  def mult(setA: FuzzySet, setB: FuzzySet): FuzzySet = {
    val mapA = elementMap(setA)
    val mapB = elementMap(setB)

    val allKeys = mapA.keySet ++ mapB.keySet
    val newElements = allKeys.map { key =>
      val valueA = mapA.getOrElse(key, 0.0)
      val valueB = mapB.getOrElse(key, 0.0)
      Element(key, round(valueA * valueB))
    }.toList

    FuzzySet(s"${setA.name}_MULT_${setB.name}", newElements)
  }

  def xor(setA: FuzzySet, setB: FuzzySet): FuzzySet = {
    val mapA = elementMap(setA)
    val mapB = elementMap(setB)

    val allKeys = mapA.keySet ++ mapB.keySet
    val newElements = allKeys.map { key =>
      val valueA = mapA.getOrElse(key, 0.0)
      val valueB = mapB.getOrElse(key, 0.0)
      Element(key, round(Math.max(valueA, valueB) - Math.min(valueA, valueB)))
    }.toList

    FuzzySet(s"${setA.name}_XOR_${setB.name}", newElements)
  }

  def alphaCut(setA: FuzzySet, alpha: Double): List[String] = {
    setA.elements.collect {
      case Element(nameA, valueA) if valueA >= alpha => nameA
    }
  }
}

object FuzzyLogicDSL {
  var gates: Map[String, FuzzyGate] = Map()
  var inputScope: Map[String, Map[String, Double]] = Map()

  def Assign(gate: FuzzyGate): Unit = {
    gates += (gate.name -> gate)
  }

  def AssignInput(gateName: String, input: Input, value: Double): Unit = {
    val currentScope = inputScope.getOrElse(gateName, Map())
    inputScope += (gateName -> (currentScope + (input.name -> value)))
  }

  def Scope(gate: FuzzyGate, inputAssignment: (Input, Double)): Unit = {
    AssignInput(gate.name, inputAssignment._1, inputAssignment._2)
  }

  def TestGate(gateName: String, expectedResult: Double): Boolean = {
    val gate = gates.getOrElse(gateName, throw new IllegalArgumentException(s"Gate $gateName not found"))
    val inputValues = inputScope.getOrElse(gateName, throw new IllegalArgumentException(s"Input values for $gateName not found"))
    val result = FuzzyGateEvaluator.evaluate(gate, inputValues)
    result == expectedResult
  }
}

// Example Usage for Gates and Sets
object Main extends App {
  import FuzzyLogicDSL._

  val gate1 = FuzzyGate("logicGate1", ADD(MULT(Input("A"), Input("B")), Input("C")))
  Assign(gate1)

  Scope(gate1, (Input("A"), 0.5))
  Scope(gate1, (Input("B"), 0.7))
  Scope(gate1, (Input("C"), 0.2))

  val testResult = TestGate("logicGate1", expectedResult = 0.55) // 0.5 * 0.7 + 0.2 = 0.55
  println(s"Test result for logicGate1: $testResult")

  val compositeGate = FuzzyGate("compositeGate", XOR(gate1.operation, Input("C")))
  Assign(compositeGate)

  try {
    TestGate("compositeGate", 0.2)
  } catch {
    case e: IllegalArgumentException => println(e.getMessage)
  }

  val setA = FuzzySet("A", List(Element("x1", 0.5), Element("x2", 0.7)))
  val setB = FuzzySet("B", List(Element("x1", 0.3), Element("x3", 0.6)))

  val xorResult = FuzzySetOperations.xor(setA, setB)
  println(s"XOR result: ${xorResult.eval()}")

  val addResult = FuzzySetOperations.add(setA, setB)
  println(s"ADD result: ${addResult.eval()}")

  val multResult = FuzzySetOperations.mult(setA, setB)
  println(s"MULT result: ${multResult.eval()}")

  val unionResult = FuzzySetOperations.union(setA, setB)
  println(s"UNION result: ${unionResult.eval()}")

  val intersectionResult = FuzzySetOperations.intersection(setA, setB)
  println(s"INTERSECTION result: ${intersectionResult.eval()}")

  val complementResult = FuzzySetOperations.complement(setA)
  println(s"COMPLEMENT result: ${complementResult.eval()}")

  val alphaCutResult = FuzzySetOperations.alphaCut(setA, 0.6)
  println(s"α-cut(0.6) result: $alphaCutResult")
}