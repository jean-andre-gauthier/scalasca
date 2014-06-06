package lara.epfl.scalasca.tests.unit.rules

import org.junit._

class StandardTests
//	extends BlockConstantPropagationTest
//	with UnusedCodeRemovalTest
//	extends DoubleTripleEqualsTest
//	extends PublicMutableFieldsTest
//	extends UselessAssignmentTest
//	extends DivisionByZeroTest
//	extends EmptyFinallyTest
//	with IntraProceduralControlFlowGraphTest
	extends UnfreedResourcesTest
	{

	//Allows eclipse to recognise class as unit test class
	@Test def myTest = ()
}