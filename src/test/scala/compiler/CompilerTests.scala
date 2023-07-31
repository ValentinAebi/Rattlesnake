package compiler

import compiler.io.SourceFile
import org.junit.Assert.{assertArrayEquals, assertEquals, assertFalse, assertTrue}
import org.junit.{After, Before, Test}

import java.io.*
import java.net.URLClassLoader
import java.nio.file.{Files, Path}
import java.util.spi.ToolProvider
import scala.reflect.ClassTag
import scala.util.Using
import org.objectweb.asm.Opcodes.V1_8

class CompilerTests {

  private val tmpTestDir = "testtmp"
  private val javaVersionCode = V1_8

  @After
  def deleteTmpDir(): Unit = {
    deleteRecursively(new File(tmpTestDir))
  }

  @Test
  def geometryTest(): Unit = {
    val xs = Array(1.2, 7.5, 9.3)
    val ys = Array(4.7, 14.1, 5.6)
    val ws = Array(9.3, 7.9, 7.1)
    val hs = Array(7.0, 6.9, 11.8)
    val rectDims = Array(xs(0), ys(0), ws(0), hs(0), xs(1), ys(1), ws(1), hs(1), xs(2), ys(2), ws(2), hs(2))
    val areas = new Array[Double](3)
    val actualRes = compileAndExecOneIter("geometry", "createRectangles", rectDims, areas)
    assertTrue(actualRes.isInstanceOf[Array[_]])
    val actualResArray = actualRes.asInstanceOf[Array[_]]
    assertEquals(3, actualResArray.length)
    val expectedAreas = Array(9.3 * 7.0, 7.9 * 6.9, 7.1 * 11.8)
    assertArrayEquals(expectedAreas, areas, 0.05)

    def assertRectangle(rectangle: Any, x: Double, y: Double, width: Double, height: Double): Unit = {
      assertEquals(width, getFieldValue(rectangle, "width"))
      assertEquals(height, getFieldValue(rectangle, "height"))
      val vertex = getFieldValue(rectangle, "upperLeftCorner")
      assertEquals(x, getFieldValue(vertex, "x"))
      assertEquals(y, getFieldValue(vertex, "y"))
    }

    for (rect, i) <- actualResArray.zipWithIndex do {
      assertRectangle(rect, xs(i), ys(i), ws(i), hs(i))
    }
  }

  @Test
  def sortingTest(): Unit = {
    val inputArray = Array(12, 25, -11, 47, 21, 9, 37, 64, 78, 95, 87, 32, 40, 31, 90)
    val expectedRes = inputArray.sorted.reverse
    compileAndExecOneIter("sorting", "selectionSort", inputArray)
    assertArrayEquals(expectedRes, inputArray)
  }

  @Test
  def stringgenTest(): Unit = {
    val actRes = compileAndExecOneIter("stringgen", "generateString")
    assertEquals("Hello!", actRes)
  }

  @Test
  def comparisonsTest(): Unit = {

    def scalaImplementation(x: Int): Int = {
      if (x < 0) {
        if (x >= -5) {
          -12
        } else if (x > -11) {
          if (x != -7){
            -742
          } else {
            1521
          }
        } else {
          207
        }
      } else if (x == 0) {
        -75
      } else {
        if ((x > 25 && x <= 35) || x == 100) {
          100
        } else {
          1005
        }
      }
    }

    val range = -50 to 150
    val actualResults = compileAndExecSeveralIter("comparisons", "testFunc", range.map(Array[Any](_)).toList)
    for i <- range do {
      val exp = scalaImplementation(i)
      val act = actualResults(i-range.start)
      assertEquals(exp, act)
    }
  }

  @Test
  def multiplicationTableTest(): Unit = {
    val actRes = compileAndExecOneIter("multable", "createMultiplicationTable", 50)
    assertTrue(actRes.isInstanceOf[Array[Array[Int]]])
    val actResArray = actRes.asInstanceOf[Array[Array[Int]]]
    val expRes = Array.tabulate(50, 50)((i, j) => (i + 1) * (j + 1))
    for i <- 0 until 50 do {
      assertArrayEquals(expRes(i), actResArray(i))
    }
  }

  @Test
  def modifStructTest(): Unit = {
    val input = Array(12, 25, 33, 41, 28, 22, 27, 91, 0, -7, 14, -5, 9)
    val actRes = compileAndExecOneIter("modifstruct", "testFunc", input)
    assertTrue(actRes.isInstanceOf[Array[Int]])
    val actResArray = actRes.asInstanceOf[Array[Int]]
    val evenCnt = input.count(_ % 2 == 0)
    val oddCnt = input.length - evenCnt
    val expRes = Array(evenCnt, oddCnt)
    assertArrayEquals(expRes, actResArray)
  }

  @Test
  def voidReturnTest(): Unit = {
    val actRes = compileAndExecOneIter("void_ret", "main")
    assertTrue(actRes.isInstanceOf[Array[Int]])
    val actResArray = actRes.asInstanceOf[Array[Int]]
    val expRes = Array(-10, -20, 42)
    assertArrayEquals(expRes, actResArray)
  }

  @Test
  def lazyValTest(): Unit = {
    val actRes = compileAndExecOneIter("lazyval", "testFunc")
    assertTrue(actRes.isInstanceOf[Array[Boolean]])
    val actResArray = actRes.asInstanceOf[Array[Boolean]]
    assertEquals(4, actResArray.length)
    assertTrue(actResArray(0))
    assertFalse(actResArray(1))
    assertTrue(actResArray(2))
    assertTrue(actResArray(3))
  }

  @Test
  def stringConcatTest(): Unit = {

    def refImpl(input: Array[String]): String = {
      input(0) ++ " " ++ input(1)
    }

    val inputs = List(
      Array("foo", "bar"),
      Array("programming", "language"),
      Array("baz", ""),
      Array("\\n", "newline")
    )
    val actRes = compileAndExecSeveralIter("stringconcat", "testF", inputs)
    for (exp, act) <- inputs.map(refImpl).zip(actRes) do {
      assertEquals(exp, act)
    }
  }

  @Test
  def returnVoidTest(): Unit = {
    val argArray = new Array[Int](2)   // content should be written by the function under test
    compileAndExecOneIter("voidret", "main", argArray)
    assertEquals(argArray(0), -15)
    assertEquals(argArray(1), 20)
  }

  @Test
  def arrayLitTest(): Unit = {
    val res = compileAndExecOneIter("arraylit", "createArray")
    assertTrue(res.isInstanceOf[Array[Double]])
    val resArray = res.asInstanceOf[Array[Double]]
    val exp = Array(361.0, 14.31, -2.1, 11+9.4, -5.0*28, 94.35*32.21)
    val tol = 1e-8
    assertArrayEquals(exp, resArray, tol)
  }

  @Test
  def toCharArrayTest(): Unit = {
    val inputs = List("abcde", "Programming language", "Rattle-snake")
    val res = compileAndExecSeveralIter("strtochars", "testFunc", inputs.map(Array(_)))
    assertEquals(inputs.length, res.length)
    assertTrue(res.forall(_.isInstanceOf[Array[Char]]))
    for (exp, act) <- inputs.zip(res) do {
      assertArrayEquals(exp.toCharArray, act.asInstanceOf[Array[Char]])
    }
  }

  @Test
  def equalityTest(): Unit = {
    val res = compileAndExecOneIter("equality", "testFunc")
    assertTrue(res.isInstanceOf[Array[Boolean]])
    val exp = Array(true, true, false, true, true, false, true, false, false, true)
    def convert(b: Boolean) = if b then 1 else 0
    assertArrayEquals(exp.map(convert), res.asInstanceOf[Array[Boolean]].map(convert))
  }

  @Test
  def strLenTest(): Unit = {
    val inputs = List("programming language", "foo", "", "compilation")
    val results = compileAndExecSeveralIter("stringlen", "testFunc", inputs.map(Array(_)))
    for (exp, act) <- inputs.map(_.length).zip(results) do {
      assertEquals(exp, act)
    }
  }

  @Test
  def castTest(): Unit = {
    val res = compileAndExecOneIter("typecasts", "testFunc")
    assertEquals("a b19512.8912", res)
  }

  @Test
  def doubleComparisonTest(): Unit = {

    def refImpl(xs: Array[Double]) = {
      require(xs.length == 2)
      xs(0) >= xs(1)
    }

    val inputs = List(Array(14.71, -9.48), Array(11.11, 72.21), Array(-8.9, 0.5), Array(7.7, 7.7))
    val res = compileAndExecSeveralIter("doublecomp", "isGreaterOrEqual", inputs.map(Array(_)))
    inputs.map(refImpl).zip(res).foreach { (exp, act) =>
      assertEquals(exp, act)
    }
  }

  @Test
  def ternaryOperatorTest(): Unit = {
    val arr1 = Array(12.7, 9.47, 11.2, 15.8, 19.8)
    val arr2 = Array(74.82, -71.68, -11.45, 15.21, 9.999, -9.999)
    val inputs = List(Array(arr1, "min"), Array(arr1, "max"), Array(arr2, "max"), Array(arr2, "min"))
    val actualRes = compileAndExecSeveralIter("maxmin", "testFunc", inputs)
    val expectedRes = List(arr1.min, arr1.max, arr2.max, arr2.min)
    assertEquals(expectedRes, actualRes)
  }

  @Test
  def multiDimArrayTest(): Unit = {
    val res = compileAndExecOneIter("muldimarray", "testF")
    assertTrue(res.isInstanceOf[Array[Array[_]]])
    val resArray = res.asInstanceOf[Array[Array[_]]]
    assertEquals(1, resArray.length)
    assertEquals(1, resArray(0).length)
  }

  @Test def constantsTest(): Unit = {
    val inputs = List(41, 807, 553, 552)
    val rawActualRes = compileAndExecSeveralIter("constants", "testFunc",
      inputs.map(i => Array(Array(i)))
    )
    assertTrue(rawActualRes.forall(_.isInstanceOf[String]))
    val actualRes = rawActualRes.map(_.asInstanceOf[String])
    val success = "OK"
    val failure = "K.O."
    val expectedRes = List(success, failure, failure, success)
    assertEquals(expectedRes, actualRes)
  }

  private def compileAndExecOneIter(srcFileName: String, testedMethodName: String, args: Any*): Any = {
    compileAndExecSeveralIter(srcFileName, testedMethodName, List(args.toArray)).head
  }

  /**
   * @param srcFileName the name of the source file
   * @param testedMethodName the method to be called
   * @param argsPerIter a list of arrays, each containing the arguments to be provided to the tested function during an iteration
   * @return the values returnes by each iteration
   */
  private def compileAndExecSeveralIter(srcFileName: String, testedMethodName: String, argsPerIter: List[Array[_]]): List[Any] = {
    val tmpDir = Path.of(tmpTestDir, srcFileName)
    val outputName = srcFileName.withHeadUppercase + GenFilesNames.coreFilePostfix
    val compiler = TasksPipelines.compiler(tmpDir, javaVersionCode, outputName, true)
    val testFile = SourceFile(s"src/test/res/$srcFileName.${FileExtensions.rattlesnake}")
    val writtenFilesPaths = compiler.apply(List(testFile))
    val classes = {
      for path <- writtenFilesPaths yield {
        val bytes = Files.readAllBytes(path)
        val className = path.getFileName.toString.takeWhile(_ != '.')
        Loader.load(className, bytes)
      }
    }
    val coreClass = classes.find(_.getName.endsWith(GenFilesNames.coreFilePostfix)).get
    val method = coreClass.getDeclaredMethods.find(_.getName == testedMethodName).get
    for args <- argsPerIter yield {
      method.invoke(null, args: _*)
    }
  }

  private object Loader extends ClassLoader(Thread.currentThread().getContextClassLoader) {
    def load(name: String, bytes: Array[Byte]): Class[_] = {
      super.defineClass(name, bytes, 0, bytes.length)
    }
  }

  private def getFieldValue(obj: Any, fieldName: String): Any = {
    obj.getClass.getField(fieldName).get(obj)
  }

  private def deleteRecursively(file: File): Unit = {
    val subFiles = file.listFiles()
    if (subFiles != null) {
      for file <- subFiles do {
        deleteRecursively(file)
      }
    }
    Files.delete(file.toPath)
  }

  extension (str: String) private def withHeadUppercase: String = {
    if str.isEmpty then str
    else str.head.toUpper +: str.tail
  }

}
