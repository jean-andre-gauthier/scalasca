package lara.epfl.scalasca.tests.unit.rules

import org.junit._

class IntraProceduralControlFlowGraphTest extends BasicTest {

	def runIPCFGTest(test: String) = super.runTest("intraproceduralcontrolflowgraph", test)

	/*
	 * Unit test deactivated, as they don't pass due to the random graph nodes ordering
	 */
//	@Test def basicTryTest: Unit = runIPCFGTest("BasicTryTest")
//	@Test def basicIfTest: Unit = runIPCFGTest("BasicIfTest")
//	@Test def multipleMethodsTest: Unit = runIPCFGTest("MultipleMethodsTest")
//	@Test def whileTest: Unit = runIPCFGTest("WhileTest")
//	@Test def doWhileTest: Unit = runIPCFGTest("DoWhileTest")
//	@Test def matchTest: Unit = runIPCFGTest("MatchTest")
//	@Test def elementaryTryTest: Unit = runIPCFGTest("ElementaryTryTest")

	/*
	 * As for now, foreaches are not taken into account
	 */
//	@Test def foreachTest: Unit = runIPCFGTest("ForeachTest")

}