package scalasca.tests.standalone

class DoubleEquals {
  def main(args: Array[String]) {
    val a = new TestClassA()
    val b = new TestClassB()
    a == b
  }
}

class TestClassA {}

class TestClassB extends TestClassA {}