package compiler

import compiler.io.SourceFile
import org.junit.Assert.{assertArrayEquals, assertEquals, assertTrue}
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

  private def compileAndExecOneIter(srcFileName: String, testedMethodName: String, args: Any*): Any = {
    compileAndExecSeveralIter(srcFileName, testedMethodName, List(args.toArray)).head
  }

  /**
   * @param srcFileName the name of the source file
   * @param testedMethodName the method to be called
   * @param argsPerIter a list of arrays, each containing the arguments to be provided to the tested function during an iteration
   * @return the values returnes by each iteration
   */
  private def compileAndExecSeveralIter(srcFileName: String, testedMethodName: String, argsPerIter: List[Array[Any]]): List[Any] = {
    val tmpDir = Path.of(tmpTestDir, srcFileName)
    val outputName = srcFileName.withHeadUppercase + "_core"
    val compiler = TasksPipelines.compiler(tmpDir, javaVersionCode, outputName)
    val testFile = SourceFile(s"src/test/res/$srcFileName.${FileExtensions.rattlesnake}")
    val writtenFilesPaths = compiler.apply(List(testFile))
    val classes = {
      for path <- writtenFilesPaths yield {
        val bytes = Files.readAllBytes(path)
        val className = path.getFileName.toString.takeWhile(_ != '.')
        Loader.load(className, bytes)
      }
    }
    val coreClass = classes.find(_.getName.endsWith("_core")).get
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
