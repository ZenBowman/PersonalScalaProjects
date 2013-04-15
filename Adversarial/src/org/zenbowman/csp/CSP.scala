package org.zenbowman.csp

import scala.collection.mutable

class Variable(val name: String) {
  override def equals(obj: Any): Boolean = {
    if (obj.isInstanceOf[Variable]) {
      return name == obj.asInstanceOf[Variable].name
    }
    false
  }
}

class VariableAssignment[T](val variable: Variable, val value: T)

abstract class Constraint[T] {
  def satisfied(assignmentSet: AssignmentSet[T]): Boolean
}

class AssignmentSet[T](val assignments: List[VariableAssignment[T]]) {

  def getAssignmentFor(variableName: String): Option[T] = getAssignmentFor(new Variable(variableName))

  def getAssignmentFor(v: Variable): Option[T] = {
    for (va <- assignments; if va.variable == v) {
      return Some(va.value)
    }
    None
  }

  def hasAssignmentFor(v: Variable): Boolean = getAssignmentFor(v) != None

  def withAssignment(variableAssignment: VariableAssignment[T]): AssignmentSet[T] = {
    new AssignmentSet(variableAssignment :: assignments)
  }
}

abstract class CSP[T](val variables: List[Variable], constraints: List[Constraint[T]]) {
  def isComplete(assignmentSet: AssignmentSet[T]): Boolean = getUnassignedVariable(assignmentSet) == None

  def getUnassignedVariable(assignmentSet: AssignmentSet[T]): Option[Variable] = {
    for (v <- variables; if !assignmentSet.hasAssignmentFor(v)) {
      return Some(v)
    }
    None
  }

  def constraintsSatisfied(assignmentSet: AssignmentSet[T]): Boolean = {
    for (c <- constraints) {
      if (!c.satisfied(assignmentSet)) {
        return false
      }
    }
    true
  }

  def getDomainValues: List[T]
}

object BacktrackingSearch {
  def doSearch[T](csp: CSP[T]) = {
    doRecursiveSearch(csp, new AssignmentSet[T](List()))
  }

  def doRecursiveSearch[T](csp: CSP[T], assignments: AssignmentSet[T]): Option[AssignmentSet[T]] = {
    if (csp.isComplete(assignments)) {
      return Some(assignments)
    }

    val variable = csp.getUnassignedVariable(assignments).get
    for (possibleValue <- csp.getDomainValues) {
      val newAssignments = assignments.withAssignment(new VariableAssignment[T](variable, possibleValue))
      if (csp.constraintsSatisfied(newAssignments)) {
        val result = doRecursiveSearch(csp, newAssignments)
        if (result.isDefined) {
          return Some(result.get)
        }
      }
    }

    None
  }
}

class EquationConstraint(val variablesInvolved: List[Variable],
                         val equationMatched: AssignmentSet[Int] => Boolean) extends Constraint[Int] {
  def satisfied(assignmentSet: AssignmentSet[Int]): Boolean = {
    for (v <- variablesInvolved) {
      if (!assignmentSet.hasAssignmentFor(v)) {
        return true
      }
    }
    equationMatched(assignmentSet)
  }
}

class AllDiffConstraint extends Constraint[Int] {
  def satisfied(assignmentSet: AssignmentSet[Int]): Boolean = {
    val usedValues = new mutable.HashSet[Int]
    for (v <- assignmentSet.assignments) {
      if (usedValues.contains(v.value)) {
        return false
      }
      usedValues.add(v.value)
    }
    true
  }
}


