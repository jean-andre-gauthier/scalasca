package lara.epfl.scalasca.tests.unit.rules

import scala.reflect.runtime.universe._

import org.junit._

trait BlockConstantPropagationTest extends BasicTest {

	def runTest(test: String) = super.runTest("blockconstantpropagation", test)

	@Test def BCPNumAddNumTest: Unit = runTest("BlockConstantPropagationNumAddNumTest")
	@Test def BCPNumSubNumTest: Unit = runTest("BlockConstantPropagationNumSubNumTest")
	@Test def BCPNumMulNumTest: Unit = runTest("BlockConstantPropagationNumMulNumTest")
	@Test def BCPNumDivNumTest: Unit = runTest("BlockConstantPropagationNumDivNumTest")
	@Test def BCPNumModNumTest: Unit = runTest("BlockConstantPropagationNumModNumTest")
	@Test def BCPNumLeqNumTest: Unit = runTest("BlockConstantPropagationNumLeqNumTest")
	@Test def BCPNumLeNumTest: Unit = runTest("BlockConstantPropagationNumLeNumTest")
	@Test def BCPNumGeqNumTest: Unit = runTest("BlockConstantPropagationNumGeqNumTest")
	@Test def BCPNumGeNumTest: Unit = runTest("BlockConstantPropagationNumGeNumTest")
	@Test def BCPNumEqNumTest: Unit = runTest("BlockConstantPropagationNumEqNumTest")
	@Test def BCPNumNeqNumTest: Unit = runTest("BlockConstantPropagationNumNeqNumTest")

	@Test def BCPIntegerAndIntegerTest: Unit = runTest("BlockConstantPropagationIntegerAndIntegerTest")
	@Test def BCPIntegerOrIntegerTest: Unit = runTest("BlockConstantPropagationIntegerOrIntegerTest")
	@Test def BCPIntegerXorIntegerTest: Unit = runTest("BlockConstantPropagationIntegerXorIntegerTest")

	@Test def BCPBooleanAndBooleanTest: Unit = runTest("BlockConstantPropagationBooleanAndBooleanTest")
	@Test def BCPBooleanOrBooleanTest: Unit = runTest("BlockConstantPropagationBooleanOrBooleanTest")
	@Test def BCPBooleanXorBooleanTest: Unit = runTest("BlockConstantPropagationBooleanXorBooleanTest")
	@Test def BCPBooleanEqTest: Unit = runTest("BlockConstantPropagationBooleanEqTest")
	@Test def BCPBooleanNeqTest: Unit = runTest("BlockConstantPropagationBooleanNeqTest")
	@Test def BCPBooleanNotTest: Unit = runTest("BlockConstantPropagationBooleanNotTest")

	@Test def BCPStringAddTest: Unit = runTest("BlockConstantPropagationStringAddTest")
	@Test def BCPStringLengthTest: Unit = runTest("BlockConstantPropagationStringLengthTest")

	@Test def returnTest: Unit = runTest("BlockConstantPropagationReturnTest")
	@Test def BCPIfTest: Unit = runTest("BlockConstantPropagationIfTest")

}