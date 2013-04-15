package org.zenbowman.csp

object Exercise_5_2 {
  def constraintOne(assignmentSet: AssignmentSet[Int]) = {
    val o = assignmentSet.getAssignmentFor("O").get
    val r = assignmentSet.getAssignmentFor("R").get
    (o + o) % 10 == r
  }

  def constraintTwo(assignmentSet: AssignmentSet[Int]) = {
    val o = assignmentSet.getAssignmentFor("O").get
    val r = assignmentSet.getAssignmentFor("R").get
    val x1 = ((o + o) - r) / 10
    val u = assignmentSet.getAssignmentFor("U").get
    val w = assignmentSet.getAssignmentFor("W").get
    (x1 + w + w) % 10 == u
  }

  def constraintThree(assignmentSet: AssignmentSet[Int]) = {
    val o = assignmentSet.getAssignmentFor("O").get
    val r = assignmentSet.getAssignmentFor("R").get
    val x1 = ((o + o) - r) / 10
    val u = assignmentSet.getAssignmentFor("U").get
    val w = assignmentSet.getAssignmentFor("W").get
    val x2 = ((x1 + w + w) - u) / 10
    val t = assignmentSet.getAssignmentFor("T").get
    (x2 + t + t) % 10 == o
  }

  def constraintFour(assignmentSet: AssignmentSet[Int]) = {
    val o = assignmentSet.getAssignmentFor("O").get
    val r = assignmentSet.getAssignmentFor("R").get
    val x1 = ((o + o) - r) / 10
    val u = assignmentSet.getAssignmentFor("U").get
    val w = assignmentSet.getAssignmentFor("W").get
    val x2 = ((x1 + w + w) - u) / 10
    val t = assignmentSet.getAssignmentFor("T").get
    val x3 = ((x2 + t + t) - o) / 10
    val f = assignmentSet.getAssignmentFor("F").get
    x3 == f
  }

  def constructProblem() = {
    val variableF = new Variable("F")
    val variableT = new Variable("T")
    val variableU = new Variable("U")
    val variableW = new Variable("W")
    val variableR = new Variable("R")
    val variableO = new Variable("O")
    val c1 = new EquationConstraint(List(variableO, variableR), constraintOne)
    val c2 = new EquationConstraint(List(variableO, variableR, variableW, variableU), constraintTwo)
    val c3 = new EquationConstraint(List(variableO, variableR, variableW, variableU, variableT), constraintThree)
    val c4 = new EquationConstraint(List(variableO, variableR, variableW, variableU, variableT, variableF), constraintFour)
    val c5 = new AllDiffConstraint()
    new CSP[Int](List(variableO, variableR, variableW, variableU, variableT, variableF),
      List(c1, c2, c3, c4, c5)) {
      def getDomainValues: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    }
  }

  def displayResult(assignmentSet: AssignmentSet[Int]) {
    for {
      va <- assignmentSet.assignments
      vname = va.variable.name
      value = va.value
    } {
      println("%s = %s".format(vname, value))
    }
  }

  def main(args: Array[String]) {
    val ex_5_2 = constructProblem()
    val result = BacktrackingSearch.doSearch(ex_5_2)
    displayResult(result.get)
  }
}
