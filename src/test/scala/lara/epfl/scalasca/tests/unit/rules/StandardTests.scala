package lara.epfl.scalasca.tests.unit.rules

import org.junit._

class StandardTests extends BlockConstantPropagationTest with CodeRemovalTest with UselessAssignmentTest {

	//Allows eclipse to recognise class as unit test class
	@Test def myTest = ()
}