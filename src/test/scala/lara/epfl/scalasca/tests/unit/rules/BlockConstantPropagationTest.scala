package lara.epfl.scalasca.tests.unit.rules

import scala.reflect.runtime.universe._

import org.junit._

trait BlockConstantPropagationTest extends BasicTest {

	def runBCPTest(test: String) = super.runTest("blockconstantpropagation", test)

	@Test def BCPNumAddNumTest: Unit = runBCPTest("BlockConstantPropagationNumAddNumTest")
	@Test def BCPNumSubNumTest: Unit = runBCPTest("BlockConstantPropagationNumSubNumTest")
	@Test def BCPNumMulNumTest: Unit = runBCPTest("BlockConstantPropagationNumMulNumTest")
	@Test def BCPNumDivNumTest: Unit = runBCPTest("BlockConstantPropagationNumDivNumTest")
	@Test def BCPNumModNumTest: Unit = runBCPTest("BlockConstantPropagationNumModNumTest")
	@Test def BCPNumLeqNumTest: Unit = runBCPTest("BlockConstantPropagationNumLeqNumTest")
	@Test def BCPNumLeNumTest: Unit = runBCPTest("BlockConstantPropagationNumLeNumTest")
	@Test def BCPNumGeqNumTest: Unit = runBCPTest("BlockConstantPropagationNumGeqNumTest")
	@Test def BCPNumGeNumTest: Unit = runBCPTest("BlockConstantPropagationNumGeNumTest")
	@Test def BCPNumEqNumTest: Unit = runBCPTest("BlockConstantPropagationNumEqNumTest")
	@Test def BCPNumNeqNumTest: Unit = runBCPTest("BlockConstantPropagationNumNeqNumTest")

	@Test def BCPIntegerAndIntegerTest: Unit = runBCPTest("BlockConstantPropagationIntegerAndIntegerTest")
	@Test def BCPIntegerOrIntegerTest: Unit = runBCPTest("BlockConstantPropagationIntegerOrIntegerTest")
	@Test def BCPIntegerXorIntegerTest: Unit = runBCPTest("BlockConstantPropagationIntegerXorIntegerTest")

	@Test def BCPBooleanAndBooleanTest: Unit = runBCPTest("BlockConstantPropagationBooleanAndBooleanTest")
	@Test def BCPBooleanOrBooleanTest: Unit = runBCPTest("BlockConstantPropagationBooleanOrBooleanTest")
	@Test def BCPBooleanXorBooleanTest: Unit = runBCPTest("BlockConstantPropagationBooleanXorBooleanTest")
	@Test def BCPBooleanEqTest: Unit = runBCPTest("BlockConstantPropagationBooleanEqTest")
	@Test def BCPBooleanNeqTest: Unit = runBCPTest("BlockConstantPropagationBooleanNeqTest")
	@Test def BCPBooleanNotTest: Unit = runBCPTest("BlockConstantPropagationBooleanNotTest")

	@Test def BCPStringAddTest: Unit = runBCPTest("BlockConstantPropagationStringAddTest")
	@Test def BCPStringLengthTest: Unit = runBCPTest("BlockConstantPropagationStringLengthTest")
//
	@Test def BCPIfTest: Unit = runBCPTest("BlockConstantPropagationIfTest")
//
//	/**
//	 * Regression tests
//	 */
	@Test def BCPReg1Test: Unit = runBCPTest("BlockConstantPropagationReg1Test")
}